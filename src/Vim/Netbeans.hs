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
, setUnmodified
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
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
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

type Netbeans = ReaderT ConnState

data ConnState = ConnState
    { sequenceCounter :: MVar Int
    , connHandle      :: MVar Handle
    , bufNumber       :: MVar Int
    , annoTypeNumber  :: MVar Int
    , annoNumber      :: MVar Int
    , protVersion     :: String
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

-- preflight
    message <- liftIO $ atomically $ readTChan q
    version <- liftIO $ pollVersion q
    P.EventMessage _ _ P.StartupDone <- liftIO $ atomically $ readTChan q
-- end preflight
    runReaderT vm (ConnState
                        seqCounter
                        hMVar
                        bufMVar
                        annoTypeMVar
                        annoMVar
                        version
                        q
                        pm)
    return ()

pollVersion :: TChan P.VimMessage -> IO String
pollVersion q = do
    P.EventMessage _ _ (P.Version version) <- atomically $ readTChan q
    return version

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
    q <- messageQueue `liftM` ask
    message <- liftIO $ atomically $ readTChan q
    case message of
        P.EventMessage bufId seqNo event -> return (bufId, event)
        P.ReplyMessage _ _ -> nextEvent

{- | Returns Just next event from the event queue or Nothing if no events
available. -}
tryNextEvent :: MonadIO m => Netbeans m (Maybe (P.BufId, P.Event))
tryNextEvent = do
    q <- messageQueue `liftM` ask
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

yieldCommandNumber :: MonadIO m => Netbeans m Int
yieldCommandNumber = do
    st <- ask
    let seqNoMVar = sequenceCounter st
    seqNo <- liftIO $ takeMVar seqNoMVar
    liftIO $ putMVar seqNoMVar (seqNo + 1)
    return seqNo

yieldBufferId :: MonadIO m => Netbeans m P.BufId
yieldBufferId = do
    st <- ask
    let bufNumberMVar = bufNumber st
    bufNo <- liftIO $ takeMVar bufNumberMVar
    liftIO $ putMVar bufNumberMVar (bufNo + 1)
    return $ P.BufId bufNo

yieldAnnoTypeNum :: MonadIO m => Netbeans m P.AnnoTypeNum
yieldAnnoTypeNum = do
    st <- ask
    let annoMVar = annoTypeNumber st
    annoNo <- liftIO $ takeMVar annoMVar
    liftIO $ putMVar annoMVar (annoNo + 1)
    return $ P.AnnoTypeNum annoNo

yieldAnnoNum :: MonadIO m => Netbeans m P.AnnoNum
yieldAnnoNum = do
    st <- ask
    let annoMVar = annoNumber st
    annoNo <- liftIO $ takeMVar annoMVar
    liftIO $ putMVar annoMVar (annoNo + 1)
    return $ P.AnnoNum annoNo

sendCommand :: MonadIO m => P.BufId -> P.Command -> Netbeans m ()
sendCommand bufId cmdMsg = do
    seqNo <- yieldCommandNumber
    hMVar <- connHandle `liftM` ask
    let message = P.printMessage $ P.CommandMessage bufId seqNo cmdMsg
    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h

sendFunction :: MonadIO m => P.BufId
                          -> P.Function
                          -> Parser P.Reply
                          -> Netbeans m P.Reply
sendFunction bufId funcMsg parser = do
    seqNo <- yieldCommandNumber

    hMVar <- connHandle `liftM` ask
    q <- messageQueue `liftM` ask
    mq <- liftIO $ atomically $ dupTChan q

    pm <- parserMap `liftM` ask
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
    annoId <- yieldAnnoNum
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
    annoTypeId <- yieldAnnoTypeNum
    sendCommand bufId $
                    P.DefineAnnoType
                            annoTypeId
                            typeName
                            toolTip
                            glyphFile
                            fg
                            bg
    return $ annoTypeId

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
    bufId <- yieldBufferId
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

{- | Associate a buffer number with the Vim buffer by the name
pathname, a string argument.  To be used when the editor
reported editing another file to the IDE and the IDE needs to
tell the editor what buffer number it will use for this file.
Also marks the buffer as initialized.

New in version 2.1.
-}
putBufferNumber :: MonadIO m => String -- ^ pathname
                             -> Netbeans m P.BufId -- ^ buffer id
putBufferNumber pathname = do
    bufId <- yieldBufferId
    sendCommand bufId $ P.PutBufferNumber pathname
    return bufId

{- | Bring the editor to the foreground. Only when Vim is run with GUI.

New in protocol version 2.1 -}
raise :: MonadIO m => P.BufId -> Netbeans m ()
raise bufId =
    sendCommand bufId $ P.Raise

{- | Remove a previously placed annotation for this buffer.
serNum is the same number used in addAnno. -}
removeAnno :: MonadIO m => P.BufId -> P.AnnoNum -> Netbeans m ()
removeAnno bufId annoNum =
    sendCommand bufId $ P.RemoveAnno annoNum

{- | Save the buffer when it was modified.  The other side of the
interface is expected to write the buffer and invoke setModified to reset
the changed flag of the buffer. The writing is skipped when one of these
conditions is true:

* 'write' is not set

* the buffer is read-only

* the buffer does not have a file name

* 'buftype' disallows writing

New in version 2.2.
-}
save :: MonadIO m => P.BufId -> Netbeans m ()
save bufId =
    sendCommand bufId $ P.Save

{- | Sent by Vim Controller to tell Vim a save is done.  This triggers a save
message being printed. Prior to protocol version 2.3, no save messages were
displayed after a save.

New in protocol version 2.3.
-}
saveDone :: MonadIO m => P.BufId -> Netbeans m ()
saveDone bufId =
    sendCommand bufId $ P.SaveDone

{- | Associate a buffer number with Vim buffer by the name pathname.
To be used when the editor reported editing another file to the IDE and
the IDE needs to tell the editor what buffer number it will use for this file.
Has the side effect of making the buffer the current buffer.
See putBufferNumber for a more useful command.
-}
setBufferNumber :: MonadIO m => String -> Netbeans m P.BufId
setBufferNumber pathname = do
    bufId <- yieldBufferId
    sendCommand bufId $ P.SetBufferNumber pathname
    return bufId

{- | Make the buffer the current buffer and set the cursor at the specified
position.  If the buffer is open in another window than make that window the
current window. If there are folds they are opened to make the cursor
line visible.

In protocol version 2.1 lnum/col can be used instead of off.
However this function doesn't support lnum/col.
-}
setDot :: MonadIO m => P.BufId -> Int -> Netbeans m ()
setDot bufId off =
    sendCommand bufId $ P.SetDot off

{- | Set the delay for exiting to seconds, a number.  This delay is used to
give the IDE a chance to handle things before really exiting.
The default delay is two seconds.

New in protocol version 2.1.

Obsolete in protocol version 2.3.
-}
setExitDelay :: MonadIO m => Int -> Netbeans m ()
setExitDelay seconds =
    sendCommand (P.BufId 0) $ P.SetExitDelay seconds

{- | Set the file name to be used for a buffer to pathname, a string argument.
Used when the IDE wants to edit a file under control of the IDE.
This makes the buffer the current buffer, but does not read the file.
insert commands will be used next to set the contents.
-}
setFullName :: MonadIO m => P.BufId -> String -> Netbeans m ()
setFullName bufId pathname =
    sendCommand bufId $ P.SetFullName pathname

-- | Mark the buffer as modified.
setModified :: MonadIO m => P.BufId -> Netbeans m ()
setModified bufId =
    sendCommand bufId $ P.SetModified True

-- | Mark the buffer as unmodified.
setUnmodified :: MonadIO m => P.BufId -> Netbeans m ()
setUnmodified bufId =
    sendCommand bufId $ P.SetModified False

{- | Set a file as readonly.

New in protocol version 2.3.
-}
setReadonly :: MonadIO m => P.BufId -> Netbeans m ()
setReadonly bufId =
    sendCommand bufId $ P.SetReadOnly

{- | Set the title for the buffer to name, a string argument. The title is
only used for the Vim Controller functions, not by Vim.
-}
setTitle :: MonadIO m => P.BufId -> String -> Netbeans m ()
setTitle bufId name =
    sendCommand bufId $ P.SetTitle name

-- | Go to the buffer.
setVisible :: MonadIO m => P.BufId -> Netbeans m ()
setVisible bufId =
    sendCommand bufId $ P.SetVisible True

{- | Show a balloon (popup window) at the mouse pointer position, containing
text, a string argument. The balloon should disappear when the mouse is moved
more than a few pixels. Only when Vim is run with a GUI.

New in protocol version 2.1.
-}
showBalloon :: MonadIO m => P.BufId -> String -> Netbeans m ()
showBalloon bufId text =
    sendCommand bufId $ P.ShowBalloon text

{- | Map a set of keys (mostly function keys) to be passed back to the Vim
Controller for processing.  This lets regular IDE hotkeys be used from Vim.

Implemented in protocol version 2.3.
-}
specialKeys :: MonadIO m => P.BufId -> Netbeans m ()
specialKeys bufId =
    sendCommand bufId $ P.SpecialKeys

{- | Begin an atomic operation. The screen will not be updated until
endAtomic is given.
-}
startAtomic :: MonadIO m => P.BufId -> Netbeans m ()
startAtomic bufId =
    sendCommand bufId $ P.StartAtomic

{- | Mark the buffer to report changes to the IDE with the insert and
remove events.  The default is to report changes.
-}
startDocumentListen :: MonadIO m => P.BufId -> Netbeans m ()
startDocumentListen bufId =
    sendCommand bufId $ P.StartDocumentListen

{- | Mark the buffer to stop reporting changes to the IDE. Opposite of
startDocumentListen. NOTE: if netbeansBuffer was used to mark this buffer as a
NetBeans buffer, then the buffer is deleted in Vim. This is for compatibility
with Sun Studio 10.
-}
stopDocumentListen :: MonadIO m => P.BufId -> Netbeans m ()
stopDocumentListen bufId =
    sendCommand bufId $ P.StopDocumentListen

{- | Opposite of guard, remove guarding for a text area. Also sets the
current buffer, if necessary. -}
unguard :: MonadIO m => P.BufId -> Int -> Int -> Netbeans m ()
unguard bufId off len =
    sendCommand bufId $ P.Unguard off len

{- | Return the current buffer and cursor position.

bufID = buffer ID of the current buffer (if this is unknown -1 is used)
lnum  = line number of the cursor (first line is one)
col   = column number of the cursor (in bytes, zero based)
off   = offset of the cursor in the buffer (in bytes)

New in version 2.1.
-}
getCursor :: MonadIO m => Netbeans m (P.BufId, Int, Int, Int)
getCursor = do
    P.GetCursorReply bufId lnum col off <- sendFunction
                                                    (P.BufId 0)
                                                    P.GetCursor
                                                    P.getCursorReplyParser
    -- TODO clarify situation with -1 buf id
    return (bufId, lnum, col, off)


-- | Return the length of the buffer in bytes.
getLength :: MonadIO m => P.BufId -> Netbeans m Int
getLength bufId = do
    P.GetLengthReply value <- sendFunction
                                    bufId
                                    P.GetLength
                                    P.getLengthReplyParser
    return value

-- | Return the line number of the annotation in the buffer.
getAnno :: MonadIO m => P.BufId -> Int -> Netbeans m Int
getAnno bufId serNum = do
    P.GetAnnoReply lnum <- sendFunction
                              bufId
                              (P.GetAnno serNum)
                              P.getAnnoReplyParser
    return lnum

{- | Return the number of buffers with changes.
When the result is zero it's safe to tell Vim to exit.

New in protocol version 2.1.
-}
getModified :: MonadIO m => Netbeans m Int
getModified = do
    P.GetModifiedReply count <- sendFunction
                                    (P.BufId 0)
                                    P.GetModified
                                    P.getModifiedReplyParser
    return count

{- | Return zero if the buffer does not have changes,
one if it does have changes.  

New in protocol version 2.1.
-}
getModifiedBuffer :: MonadIO m => P.BufId -> Netbeans m Bool
getModifiedBuffer bufId = do
    P.GetModifiedReply count <- sendFunction
                                    bufId
                                    P.GetModified
                                    P.getModifiedReplyParser
    return $ case count of
        0 -> False
        _ -> True

-- | Return the contents of the buffer as a string.
getText :: MonadIO m => P.BufId -> Netbeans m String
getText bufId = do
    P.GetTextReply text <- sendFunction
                                bufId
                                P.GetText
                                P.getTextReplyParser
    return text

{- | Insert text before position off. text is a string argument, off a number.
text should have a \\n (newline) at the end of each line. Or \\r\\n when
fileformat is dos.  When using insert in an empty buffer Vim will set
fileformat accordingly. When off points to the start of a line the text is
inserted above this line. Thus when off is zero lines are inserted before
the first line. When off points after the start of a line, possibly on the
NUL at the end of a line, the first line of text is appended to this line.
Further lines come below it.
-}
insert :: (Error e, MonadIO m, MonadError e m) => P.BufId -> Int -> String -> Netbeans m ()
insert bufId off text = do
    replyMessage <- sendFunction
                        bufId
                        (P.Insert off text)
                        P.insertReplyParser
    case replyMessage of
        P.InsertReplySuccess -> return ()
        P.InsertReplyError s -> throwError $ strMsg s

-- | Delete length bytes of text at position off. Both arguments are numbers.
remove :: (Error e, MonadIO m, MonadError e m) => P.BufId -> Int -> Int -> Netbeans m ()
remove bufId off len = do
    replyMessage <- sendFunction
                        bufId
                        (P.Remove off len)
                        P.insertReplyParser
    case replyMessage of
        P.RemoveReplySuccess -> return ()
        P.RemoveReplyError s -> throwError $ strMsg s

{- | Perform the equivalent of closing Vim: :confirm qall. If there are
no changed files or the user does not cancel the operation Vim exits and
no result is sent back. The IDE can consider closing the connection as
a successful result. If the user cancels the operation the number of modified
buffers that remains is returned and Vim does not exit.

New in protocol version 2.1.
-}
saveAndExit :: MonadIO m => Netbeans m ()
saveAndExit = do
    seqNo <- yieldCommandNumber

    hMVar <- connHandle `liftM` ask
    q <- messageQueue `liftM` ask
    mq <- liftIO $ atomically $ dupTChan q

    let message = P.printMessage $ P.FunctionMessage (P.BufId 0) seqNo P.SaveAndExit

    liftIO $ withMVar hMVar $ \h -> do
        hPutStrLn h message
        hFlush h
    return ()
