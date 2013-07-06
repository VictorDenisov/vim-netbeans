module Vim.Netbeans
where

import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift, MonadTrans)
import Control.Monad (liftM, forever)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Network (listenOn, accept, PortID)
import System.IO (hGetLine, hPutStrLn)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Control.Monad.IO.Class (MonadIO)

import Data.List (isPrefixOf, find, delete)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Control.Applicative ((<$>))
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Concurrent.MVar (putMVar, takeMVar, MVar, newMVar, withMVar)
import Control.Concurrent (forkIO)

import qualified Vim.Netbeans.Protocol as P

type Netbeans = StateT ConnState

data ConnState = ConnState
    { sequenceCounter :: MVar Int
    , connHandle      :: MVar Handle
    , protVersion     :: Maybe String
    , messageQueue    :: Chan P.VimMessage
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
    q <- liftIO $ newChan
    seqCounter <- liftIO $ newMVar 1
    hMVar <- liftIO $ newMVar handleC
    liftIO $ forkIO $ messageReader pm handleC q
    runStateT (preflight >> vm) (ConnState
                                    seqCounter
                                    hMVar
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

messageReader :: MVar P.ParserMap -> Handle -> Chan P.VimMessage -> IO ()
messageReader pm h q = forever $ do
    line <- hGetLine h
    m <- takeMVar pm
    let Right msg = P.parseMessage m line -- TODO remove fromJust
    case msg of
        P.EventMessage _ _ _ -> putMVar pm m
        P.ReplyMessage seqNo _ -> do
            let m1 = filter ((seqNo /=) . fst)  m
            putMVar pm m1
    writeChan q msg

nextEvent :: MonadIO m => Netbeans m (P.BufId, P.Event)
nextEvent = do
    q <- messageQueue `liftM` get
    message <- liftIO $ readChan q
    case message of
        P.EventMessage bufId seqNo event -> return (bufId, event)
        P.ReplyMessage _ _ -> nextEvent

takeReply :: MonadIO m => Chan P.VimMessage -> Int -> Netbeans m P.Reply
takeReply q seqNo = do
    message <- liftIO $ readChan q
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

sendCommand :: MonadIO m => Int -> P.Command -> Netbeans m ()
sendCommand bufId cmdMsg = do
    seqNo <- popCommandNumber
    hMVar <- connHandle `liftM` get
    let message = P.printMessage $ P.CommandMessage bufId seqNo cmdMsg
    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h

sendFunction :: MonadIO m => Int
                          -> P.Function
                          -> Parser P.Reply
                          -> Netbeans m P.Reply
sendFunction bufId funcMsg parser = do
    seqNo <- popCommandNumber

    hMVar <- connHandle `liftM` get
    q <- messageQueue `liftM` get
    mq <- liftIO $ dupChan q

    pm <- parserMap `liftM` get
    m <- liftIO $ takeMVar pm

    liftIO $ putMVar pm ((seqNo, parser) : m)
    let message = P.printMessage $ P.FunctionMessage bufId seqNo funcMsg

    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h
    reply <- takeReply mq seqNo
    return reply

getLength :: MonadIO m => P.BufId -> Netbeans m Int
getLength bufId = do
    P.GetLengthReply value <- sendFunction
                                    bufId
                                    P.GetLength
                                    P.getLengthReplyParser
    return value
