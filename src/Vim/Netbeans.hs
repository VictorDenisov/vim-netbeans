module Vim.Netbeans
( Netbeans
, runNetbeans
, P.Event(..)
, P.BufId
, P.Color(..)
, P.AnnoTypeNum
, P.AnnoNum
, nextEvent
, tryNextEvent
-- commands
, addAnno
, close
, create
, defineAnnoType
, editFile
, endAtomic
, guard
, initDone
, insertDone
, setNetbeansBuffer
, clearNetbeansBuffer
, putBufferNumber
, raise
, removeAnno
, save
, saveDone
, setBufferNumber
, setDot
, setExitDelay
, setFullName
, setModified
, setReadonly
, setTitle
, setVisible
, showBalloon
, specialKeys
, startAtomic
, startDocumentListen
, stopDocumentListen
, unguard
-- functions
, getCursor
, getLength
, getAnno
, getModified
, getModifiedBuffer
, getText
, insert
, remove
, saveAndExit
)
where

import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift, MonadTrans)
import Control.Monad (liftM, forever, forM_)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Network (listenOn, accept, PortID)
import System.IO (hGetContents, hPutStrLn, stderr)
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
    => PortID -- ^ Port
    -> String -- ^ Expected password
    -> Netbeans m () -- ^ Monad to run
    -> m () -- ^ Internal monad
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
messageReader pm h q = do
    contents <- hGetContents h
    let lineList = lines contents
    forM_ lineList $ \line -> do
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

{- | Returns next event from the event queue. Blocks if no events available
at the moment. -}
nextEvent :: MonadIO m => Netbeans m (P.BufId, P.Event)
nextEvent = do
    q <- messageQueue `liftM` get
    message <- liftIO $ atomically $ readTChan q
    case message of
        P.EventMessage bufId seqNo event -> return (bufId, event)
        P.ReplyMessage _ _ -> nextEvent

{- | Returns Just next event from the event queue or Nothing if no events
available. -}
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

-- | Place an annotation in the buffer.
addAnno :: MonadIO m => P.BufId -- ^ buffer id where to place the annotation
                     -> P.AnnoTypeNum -- ^ id of annotation type
                     -> Int -- ^ offset of the annotation
                     -> Netbeans m P.AnnoNum -- ^ id of the placed annotation
addAnno bufId typeNum off = do
    annoId <- popAnnoNum
    sendCommand bufId $ P.AddAnno annoId typeNum off 0
    return annoId

{- | Close the buffer. This leaves us without current buffer, very dangerous to
use -}
close :: MonadIO m => P.BufId -> Netbeans m ()
close bufId =
    sendCommand bufId $ P.Close

{- | Creates a buffer without a name. Replaces the current buffer. (it's hidden
when it was changed). The Vim Controller should use this as the first command
for a file that is being opened.  The sequence of commands could be:

    * create

    * setModified             (no effect)

    * startDocumentListen

    * setTitle

    * setFullName
-}
create :: MonadIO m => P.BufId -> Netbeans m ()
create bufId =
    sendCommand bufId $ P.Close
{- | Define a type of annotation for this buffer.
Vim will define a sign for the annotation.
When \"glyphFile\" is empty, no text sign is used (new in protocol version 2.1).
When \"glyphFile\" is one or two characters long, a text sign is defined
(new in protocol version 2.1).

Note: the annotations will be defined in sequence, and the annotation type
number is later used with addAnno.
-}
defineAnnoType :: MonadIO m => P.BufId -- ^ buffer id
                            -> String  -- ^ type name
                            -> String  -- ^ tooltip
                            -> String  -- ^ glyph file
                            -> P.Color -- ^ fg
                            -> P.Color -- ^ bg
                            -> Netbeans m P.AnnoTypeNum -- ^ type num
defineAnnoType bufId typeName toolTip glyphFile fg bg = do
    annoTypeId <- popAnnoTypeNum
    sendCommand bufId $
                    P.DefineAnnoType annoTypeId typeName toolTip glyphFile fg bg
    return annoTypeId

{- | Set the name for the buffer and edit the file path, a string argument.
Normal way for the IDE to tell the editor to edit a file.

It will trigger an event fileOpened with a bufId of 0 but the buffer
has been assigned.

If the IDE is going to pass the file text to the editor use these commands
instead:

    * setFullName

    * insert

    * initDone

New in version 2.1.
-}
editFile :: MonadIO m => String -- ^ file path
                      -> Netbeans m P.BufId -- ^ assigned buffer id
editFile path = do
    bufId <- popBufferId
    sendCommand bufId $ P.EditFile path
    return bufId

{- | End an atomic operation. After startAtomic the screen is not changed until
endAtomic. Redraws the screen when necessary. -}
endAtomic :: MonadIO m => P.BufId -> Netbeans m ()
endAtomic bufId =
    sendCommand bufId $ P.EndAtomic

{- | Mark an area in the buffer as guarded.  This means it cannot be edited.
off and len are numbers and specify the text to be guarded.
-}
guard :: MonadIO m => P.BufId -- ^ buffer id
                   -> Int -- ^ off
                   -> Int -- ^ len
                   -> Netbeans m ()
guard bufId off len =
    sendCommand bufId $ P.Guard off len

{- | Mark the buffer as ready for use. Implicitly makes the buffer the current
buffer. Fires the BufReadPost autocommand event. -}
initDone :: MonadIO m => P.BufId -> Netbeans m ()
initDone bufId =
    sendCommand bufId $ P.InitDone

{- | Sent by Vim Controller to tell Vim an initial file insert is done.
This triggers a read message being printed.  Prior to protocol version 2.3,
no read messages were displayed after opening a file.

New in protocol version 2.3. -}
insertDone :: MonadIO m => P.BufId -> Netbeans m ()
insertDone bufId =
    sendCommand bufId $ P.InsertDone

{- | After invocation the buffer is owned by netbeans.

New in protocol version 2.2 -}
setNetbeansBuffer :: MonadIO m => P.BufId -> Netbeans m ()
setNetbeansBuffer bufId =
    sendCommand bufId $ P.NetbeansBuffer True

{- | Clears netbeans' ownership of the buffer.

New in protocol version 2.2 -}
clearNetbeansBuffer :: MonadIO m => P.BufId -> Netbeans m ()
clearNetbeansBuffer bufId =
    sendCommand bufId $ P.NetbeansBuffer False

putBufferNumber :: MonadIO m => P.BufId -> String -> Netbeans m ()
putBufferNumber bufId pathname =
    sendCommand bufId $ P.PutBufferNumber pathname

raise :: MonadIO m => P.BufId -> Netbeans m ()
raise bufId =
    sendCommand bufId $ P.Raise

removeAnno :: MonadIO m => P.BufId -> P.AnnoNum -> Netbeans m ()
removeAnno bufId annoNum =
    sendCommand bufId $ P.RemoveAnno annoNum

save :: MonadIO m => P.BufId -> Netbeans m ()
save bufId =
    sendCommand bufId $ P.Save

saveDone :: MonadIO m => P.BufId -> Netbeans m ()
saveDone bufId =
    sendCommand bufId $ P.SaveDone

setBufferNumber :: MonadIO m => String -> Netbeans m P.BufId
setBufferNumber pathname = do
    bufId <- popBufferId
    sendCommand bufId $ P.SetBufferNumber pathname
    return bufId

setDot :: MonadIO m => P.BufId -> Int -> Netbeans m ()
setDot bufId off =
    sendCommand bufId $ P.SetDot off

setExitDelay :: MonadIO m => Int -> Netbeans m ()
setExitDelay seconds =
    sendCommand 0 $ P.SetExitDelay seconds

setFullName :: MonadIO m => P.BufId -> String -> Netbeans m ()
setFullName bufId pathname =
    sendCommand bufId $ P.SetFullName pathname

setModified :: MonadIO m => P.BufId -> Bool -> Netbeans m ()
setModified bufId modified =
    sendCommand bufId $ P.SetModified modified

setReadonly :: MonadIO m => P.BufId -> Netbeans m ()
setReadonly bufId =
    sendCommand bufId $ P.SetReadOnly

setTitle :: MonadIO m => P.BufId -> String -> Netbeans m ()
setTitle bufId name =
    sendCommand bufId $ P.SetTitle name

setVisible :: MonadIO m => P.BufId -> Bool -> Netbeans m ()
setVisible bufId visible =
    sendCommand bufId $ P.SetVisible visible

showBalloon :: MonadIO m => P.BufId -> String -> Netbeans m ()
showBalloon bufId text =
    sendCommand bufId $ P.ShowBalloon text

specialKeys :: MonadIO m => P.BufId -> Netbeans m ()
specialKeys bufId =
    sendCommand bufId $ P.SpecialKeys

startAtomic :: MonadIO m => P.BufId -> Netbeans m ()
startAtomic bufId =
    sendCommand bufId $ P.StartAtomic

startDocumentListen :: MonadIO m => P.BufId -> Netbeans m ()
startDocumentListen bufId =
    sendCommand bufId $ P.StartDocumentListen

stopDocumentListen :: MonadIO m => P.BufId -> Netbeans m ()
stopDocumentListen bufId =
    sendCommand bufId $ P.StopDocumentListen

unguard :: MonadIO m => P.BufId -> Int -> Int -> Netbeans m ()
unguard bufId off len =
    sendCommand bufId $ P.Unguard off len

getCursor :: MonadIO m => Netbeans m (P.BufId, Int, Int, Int)
getCursor = do
    P.GetCursorReply bufId lnum col off <- sendFunction
                                                    0
                                                    P.GetCursor
                                                    P.getCursorReplyParser
    return (bufId, lnum, col, off)

getLength :: MonadIO m => P.BufId -> Netbeans m Int
getLength bufId = do
    P.GetLengthReply value <- sendFunction
                                    bufId
                                    P.GetLength
                                    P.getLengthReplyParser
    return value

getAnno :: MonadIO m => P.BufId -> Int -> Netbeans m Int
getAnno bufId serNum = do
    P.GetAnnoReply lnum <- sendFunction
                              bufId
                              (P.GetAnno serNum)
                              P.getAnnoReplyParser
    return lnum

getModified :: MonadIO m => Netbeans m Int
getModified = do
    P.GetModifiedReply count <- sendFunction
                                    0
                                    P.GetModified
                                    P.getModifiedReplyParser
    return count

getModifiedBuffer :: MonadIO m => P.BufId -> Netbeans m Bool
getModifiedBuffer bufId = do
    P.GetModifiedReply count <- sendFunction
                                    bufId
                                    P.GetModified
                                    P.getModifiedReplyParser
    return $ case count of
        0 -> True
        _ -> False

getText :: MonadIO m => P.BufId -> Netbeans m String
getText bufId = do
    P.GetTextReply text <- sendFunction
                                bufId
                                P.GetText
                                P.getTextReplyParser
    return text

insert :: (Error e, MonadIO m, MonadError e m) => P.BufId -> Int -> String -> Netbeans m ()
insert bufId off text = do
    replyMessage <- sendFunction
                        bufId
                        (P.Insert off text)
                        P.insertReplyParser
    case replyMessage of
        P.InsertReplySuccess -> return ()
        P.InsertReplyError s -> throwError $ strMsg s

remove :: (Error e, MonadIO m, MonadError e m) => P.BufId -> Int -> Int -> Netbeans m ()
remove bufId off len = do
    replyMessage <- sendFunction
                        bufId
                        (P.Remove off len)
                        P.insertReplyParser
    case replyMessage of
        P.RemoveReplySuccess -> return ()
        P.RemoveReplyError s -> throwError $ strMsg s

saveAndExit :: MonadIO m => Netbeans m ()
saveAndExit = do
    seqNo <- popCommandNumber

    hMVar <- connHandle `liftM` get
    q <- messageQueue `liftM` get
    mq <- liftIO $ atomically $ dupTChan q

    let message = P.printMessage $ P.FunctionMessage 0 seqNo P.SaveAndExit

    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h
    return ()
