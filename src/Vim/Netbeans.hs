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
import Control.Concurrent.MVar (putMVar, takeMVar, MVar, newMVar)
import Control.Concurrent (forkIO)

import qualified Vim.Netbeans.Protocol as P

type Netbeans = StateT ConnState

data ConnState = ConnState
    { sequenceCounter :: Int
    , connHandle      :: Handle
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
    liftIO $ forkIO $ messageReader pm handleC q
    runStateT (preflight >> vm) (initialConnState handleC q pm)
    return ()

preflight :: MonadIO m => Netbeans m ()
preflight = do
    message <- nextEvent
    liftIO $ putStrLn $ show message

messageReader :: MVar P.ParserMap -> Handle -> Chan P.VimMessage -> IO ()
messageReader pm h q = forever $ do
    line <- hGetLine h
    putStrLn $ "Reading line: " ++ (show line)
    m <- takeMVar pm
    let Right msg = P.parseMessage m line -- TODO remove fromJust
    case msg of
        P.EventMessage _ _ _ -> putMVar pm m
        P.ReplyMessage seqNo _ -> do
            let m1 = filter ((seqNo /=) . fst)  m
            putMVar pm m1
    putStrLn $ "Reading message: " ++ (show msg)
    writeChan q msg

nextEvent :: MonadIO m => Netbeans m (Int, P.Event)
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

initialConnState :: Handle -> Chan P.VimMessage -> MVar P.ParserMap -> ConnState
initialConnState h q pm = ConnState 1 h Nothing q pm

type BufId = Int

popCommandNumber :: Monad m => Netbeans m Int
popCommandNumber = do
    st <- get
    let seqNo = sequenceCounter st
    put $ st { sequenceCounter = seqNo }
    return seqNo

getLength :: MonadIO m => BufId -> Netbeans m Int
getLength bufId = do
    seqNo <- popCommandNumber
    h <- connHandle `liftM` get
    q <- messageQueue `liftM` get
    mq <- liftIO $ dupChan q
    pm <- parserMap `liftM` get
    m <- liftIO $ takeMVar pm
    liftIO $ putMVar pm ((seqNo, P.getLengthReplyParser) : m)
    liftIO $ hPutStrLn h $ P.printMessage $ P.FunctionMessage bufId seqNo P.GetLength
    liftIO $ hFlush h
    P.GetLengthReply value <- takeReply mq seqNo
    return value
