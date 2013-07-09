module Vim.Netbeans
where

import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift, MonadTrans)
import Control.Monad (liftM, forever)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Network (listenOn, accept, PortID)
import System.IO (hGetLine, hPutStrLn, stderr)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Control.Monad.IO.Class (MonadIO)

import Data.List (isPrefixOf, find, delete)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan,
                                     dupTChan, isEmptyTChan, tryReadTChan)
import Control.Monad.STM (atomically)
import Control.Concurrent.MVar (putMVar, takeMVar, MVar, newMVar, withMVar)
import Control.Concurrent (forkIO)

import qualified Vim.Netbeans.Protocol as P

type Netbeans = StateT ConnState

data ConnState = ConnState
    { sequenceCounter :: MVar Int
    , connHandle      :: MVar Handle
    , bufNumber       :: MVar Int
    , annoTypeNumber  :: MVar Int
    , annoNumber      :: MVar Int
    , protVersion     :: Maybe String
    , messageQueue    :: TChan P.VimMessage
    , parserMap       :: MVar P.ParserMap
    }

runNetbeans :: (Error e, MonadIO m, MonadError e m)
    => PortID -- ^ Port.
    -> String
    -> Netbeans m () -- ^ Monad to run.
    -> m () -- ^ Internal monad.
runNetbeans port password vm = do
    s <- liftIO $ listenOn port
    (handleC, hostC, portC) <- liftIO $ accept s
    liftIO $ hSetBinaryMode handleC True
    pm <- liftIO $ newMVar []
    q <- liftIO $ atomically $ newTChan
    seqCounter <- liftIO $ newMVar 1
    hMVar <- liftIO $ newMVar handleC
    bufMVar <- liftIO $ newMVar 1
    annoTypeMVar <- liftIO $ newMVar 1
    annoMVar <- liftIO $ newMVar 1
    liftIO $ forkIO $ messageReader pm handleC q
    runStateT (preflight >> vm) (ConnState
                                    seqCounter
                                    hMVar
                                    bufMVar
                                    annoTypeMVar
                                    annoMVar
                                    Nothing
                                    q
                                    pm)
    return ()

preflight :: MonadIO m => Netbeans m ()
preflight = do
    message <- nextEvent -- first message is AUTH message
    (_, P.Version v) <- nextEvent -- second message is version message
    (_, P.StartupDone) <- nextEvent -- third message is startup done
    st <- get
    put $ st { protVersion = Just v }

messageReader :: MVar P.ParserMap -> Handle -> TChan P.VimMessage -> IO ()
messageReader pm h q = forever $ do
    line <- hGetLine h
    m <- takeMVar pm
    let msg = P.parseMessage m line
    case msg of
        Left s -> do
            putMVar pm m
            hPutStrLn stderr $ "Failed to parse message: " ++ s
        Right eventMsg@(P.EventMessage _ _ _) -> do
            putMVar pm m
            atomically $ writeTChan q eventMsg
        Right funcMsg@(P.ReplyMessage seqNo _) -> do
            let m1 = filter ((seqNo /=) . fst)  m
            putMVar pm m1
            atomically $ writeTChan q funcMsg

nextEvent :: MonadIO m => Netbeans m (P.BufId, P.Event)
nextEvent = do
    q <- messageQueue `liftM` get
    message <- liftIO $ atomically $ readTChan q
    case message of
        P.EventMessage bufId seqNo event -> return (bufId, event)
        P.ReplyMessage _ _ -> nextEvent

tryNextEvent :: MonadIO m => Netbeans m (Maybe (P.BufId, P.Event))
tryNextEvent = do
    q <- messageQueue `liftM` get
    message <- liftIO $ atomically $ tryReadTChan q
    case message of
        Just (P.EventMessage bufId seqNo event) -> return $ Just (bufId, event)
        Nothing -> return Nothing
        Just (P.ReplyMessage _ _) -> tryNextEvent

takeReply :: MonadIO m => TChan P.VimMessage -> Int -> Netbeans m P.Reply
takeReply q seqNo = do
    message <- liftIO $ atomically $ readTChan q
    case message of
        P.EventMessage _ _ _ -> takeReply q seqNo
        P.ReplyMessage sn reply -> if seqNo == sn
                                    then return reply
                                    else takeReply q seqNo

popCommandNumber :: MonadIO m => Netbeans m Int
popCommandNumber = do
    st <- get
    let seqNoMVar = sequenceCounter st
    seqNo <- liftIO $ takeMVar seqNoMVar
    liftIO $ putMVar seqNoMVar (seqNo + 1)
    return seqNo

popBufferId :: MonadIO m => Netbeans m P.BufId
popBufferId = do
    st <- get
    let bufNumberMVar = bufNumber st
    bufNo <- liftIO $ takeMVar bufNumberMVar
    liftIO $ putMVar bufNumberMVar (bufNo + 1)
    return bufNo

popAnnoTypeNum :: MonadIO m => Netbeans m P.AnnoTypeNum
popAnnoTypeNum = do
    st <- get
    let annoMVar = annoTypeNumber st
    annoNo <- liftIO $ takeMVar annoMVar
    liftIO $ putMVar annoMVar (annoNo + 1)
    return annoNo

popAnnoNum :: MonadIO m => Netbeans m P.AnnoNum
popAnnoNum = do
    st <- get
    let annoMVar = annoNumber st
    annoNo <- liftIO $ takeMVar annoMVar
    liftIO $ putMVar annoMVar (annoNo + 1)
    return annoNo

sendCommand :: MonadIO m => P.BufId -> P.Command -> Netbeans m ()
sendCommand bufId cmdMsg = do
    seqNo <- popCommandNumber
    hMVar <- connHandle `liftM` get
    let message = P.printMessage $ P.CommandMessage bufId seqNo cmdMsg
    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h

sendFunction :: MonadIO m => P.BufId
                          -> P.Function
                          -> Parser P.Reply
                          -> Netbeans m P.Reply
sendFunction bufId funcMsg parser = do
    seqNo <- popCommandNumber

    hMVar <- connHandle `liftM` get
    q <- messageQueue `liftM` get
    mq <- liftIO $ atomically $ dupTChan q

    pm <- parserMap `liftM` get
    m <- liftIO $ takeMVar pm

    liftIO $ putMVar pm ((seqNo, parser) : m)
    let message = P.printMessage $ P.FunctionMessage bufId seqNo funcMsg

    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h
    reply <- takeReply mq seqNo
    return reply

addAnno :: MonadIO m => P.BufId
                     -> P.AnnoTypeNum
                     -> Int
                     -> Netbeans m P.AnnoNum
addAnno bufId typeNum off = do
    annoId <- popAnnoNum
    sendCommand bufId $ P.AddAnno annoId typeNum off 0
    return annoId

close :: MonadIO m => P.BufId -> Netbeans m ()
close bufId =
    sendCommand bufId $ P.Close

create :: MonadIO m => P.BufId -> Netbeans m ()
create bufId =
    sendCommand bufId $ P.Close

defineAnnoType :: MonadIO m => P.BufId
                            -> String
                            -> String
                            -> String
                            -> P.Color
                            -> P.Color
                            -> Netbeans m P.AnnoTypeNum
defineAnnoType bufId typeName toolTip glyphFile fg bg = do
    annoTypeId <- popAnnoTypeNum
    sendCommand bufId $
                    P.DefineAnnoType annoTypeId typeName toolTip glyphFile fg bg
    return annoTypeId

editFile :: MonadIO m => String -> Netbeans m P.BufId
editFile path = do
    bufId <- popBufferId
    sendCommand bufId $ P.EditFile path
    return bufId

endAtomic :: MonadIO m => P.BufId -> Netbeans m ()
endAtomic bufId =
    sendCommand bufId $ P.EndAtomic

guard :: MonadIO m => P.BufId -> Int -> Int -> Netbeans m ()
guard bufId off len =
    sendCommand bufId $ P.Guard off len

initDone :: MonadIO m => P.BufId -> Netbeans m ()
initDone bufId =
    sendCommand bufId $ P.InitDone

insertDone :: MonadIO m => P.BufId -> Netbeans m ()
insertDone bufId =
    sendCommand bufId $ P.InsertDone

getLength :: MonadIO m => P.BufId -> Netbeans m Int
getLength bufId = do
    P.GetLengthReply value <- sendFunction
                                    bufId
                                    P.GetLength
                                    P.getLengthReplyParser
    return value

getCursor :: MonadIO m => Netbeans m (P.BufId, Int, Int, Int)
getCursor = do
    P.GetCursorReply bufId lnum col off <- sendFunction
                                                    0
                                                    P.GetCursor
                                                    P.getCursorReplyParser
    return (bufId, lnum, col, off)

getAnno :: MonadIO m => P.BufId -> Int -> Netbeans m Int
getAnno bufId serNum = do
    P.GetAnnoReply lnum <- sendFunction
                              bufId
                              (P.GetAnno serNum)
                              P.getAnnoReplyParser
    return lnum
