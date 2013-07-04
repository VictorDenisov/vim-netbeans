module Vim.Netbeans
where

import GHC.IO.Handle (Handle, hClose, hSetBinaryMode, hPutStr, hFlush)
import Control.Monad.Trans (liftIO, lift, MonadTrans)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Network (listenOn, accept, PortID)
import System.IO (hGetLine)
import Control.Monad.Error (ErrorT, runErrorT, MonadError(..), Error(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

type Netbeans = StateT ConnState

data ConnState = ConnState
    { sequenceCounter :: Int
    , connHandle      :: Handle
    , protVersion     :: Maybe String
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
    line <- liftIO $ hGetLine handleC
    let message = parseMessage [] line
    liftIO $ putStrLn $ show message
    runStateT vm (initialConnState handleC)
    return ()

initialConnState :: Handle -> ConnState
initialConnState h = ConnState 0 h Nothing

data VimMessage = EventMessage Int Int Event -- buf seqNo event
                | ReplyMessage Int Reply -- seqNo
                | Auth String
                | E463
                | E532
                | E656
                | E657
                | E658
                | E744
                  deriving (Eq, Show)

data IdeMessage = CommandMessage Int Int Command -- buf seqNo command
                | FunctionMessage Int Int Function -- buf seqNo function
                | Detach
                | DisconnectCommand
                  deriving (Eq, Show)

data Reply = GetCursorReply
                Int -- bufId
                Int -- lnum
                Int -- col
                Int -- off
           | GetLengthReply
                Int -- len
             deriving (Eq, Show)

data Function = GetCursor
              | GetLength
              | GetAnno Int -- serNum
              | GetModified
              | GetText
              | Insert
                    Int -- off
                    String -- text
              | Remove
                    Int -- off
                    Int -- len
              | SaveAndExit
                deriving (Eq, Show)

data Event = FileOpened
                String -- path
                Bool -- open
                Bool -- modified
           | KeyCommand
                String -- key
           | NewDotAndMark
                Int -- off1
                Int -- off2
           | StartupDone
           | Version
                String
             deriving (Eq, Show)

data Color = Red
           | Green
           | Blue
           | Cyan
           | Magenta
           | Yellow
           | Gray
           | Black
           | Orange
           | LightRed
           | LightGreen
           | LightBlue
           | LightCyan
           | LightMagenta
           | LightYellow
           | LightGray
           | White
           | Purple
           | DarkRed
           | DardGreen
           | DarkBlue
           | DarkCyan
           | DarkMagenta
           | Brown
           | DarkGray
           | Violet
           | SeaGreen
           | SlateBlue
           | DarkYellow
             deriving (Eq, Show)

data Command = AddAnno
                Int -- serNum
                Int -- typeNum
                Int -- off
                Int -- len
             | Close
             | Create
             | DefineAnnoType
                Int -- typeNum
                String -- typeName
                String -- toolTip
                String -- glyphFile
                Color -- fg
                Color -- bg
             | EditFile
                String -- path
             | EndAtomic
             | Guard
                Int -- off
                Int -- len
             | InitDone
             | InsertDone
             | NetbeansBuffer
                Bool -- isNetbeansBuffer
             | PutBufferNumber
                String -- isNetbeansBuffer
             | Raise
             | RemoveAnno
                Int -- serNum
             | Save
             | SaveDone
             | SetBufferNumber
                String -- path
             | SetDot
                Int -- off
             | SetExitDelay
                Int -- delay seconds
             | SetFullName
                String -- fullname
             | SetModified
                Bool -- modified
             | SetReadOnly
             | SetTitle
                String -- title
             | SetVisible
                Bool -- visible
             | ShowBalloon
                String -- text
             | SpecialKeys
             | StartAtomic
             | StartDocumentListen
             | StopDocumentListen
             | Unguard
                Int -- off
                Int -- len
               deriving (Eq, Show)

type ParserMap = [(Int, Parser Reply)]

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

messageParser :: ParserMap -> Parser VimMessage
messageParser parserMap = try authParser
                      <|> try parseError
                      <|> try (regularMessageParser parserMap)

regularMessageParser :: ParserMap -> Parser VimMessage
regularMessageParser parserMap = do
    num <- parseNumber
    c <- oneOf ": "
    case c of
        ' ' -> replyParser num parserMap
        ':' -> eventParser num

replyParser :: Int -> ParserMap -> Parser VimMessage
replyParser seqno parserMap = do
    let parser = fromJust $ lookup seqno parserMap
    reply <- parser
    return $ ReplyMessage seqno reply

getCursorReplyParser :: Parser Reply
getCursorReplyParser = do
    bufId <- parseNumber
    char ' '
    lnum <- parseNumber
    char ' '
    col <- parseNumber
    char ' '
    off <- parseNumber
    return $ GetCursorReply bufId lnum col off

getLengthReplyParser :: Parser Reply
getLengthReplyParser = do
    len <- parseNumber
    return $ GetLengthReply len

eventParser :: Int -> Parser VimMessage
eventParser bufId = try (versionParser bufId)
                <|> try (startupDoneParser bufId)
                <|> try (fileOpenedParser bufId)
                <|> try (newDotAndMarkParser bufId)
                <|> try (keyCommandParser bufId)

parseError :: Parser VimMessage
parseError = do
    char 'E'
    s <- count 3 digit
    case s of
        "463" -> return $ E463 -- Region is guarded, cannot modify
        "532" -> return $ E532 -- The defineAnnoType highlighting color name is too long
        "656" -> return $ E656 -- Writes of unmodified buffers forbidden
        "657" -> return $ E657 -- Partial writes disallowed
        "658" -> return $ E658 -- Connection lost for this buffer
        "744" -> return $ E744 -- Read-only file

authParser :: Parser VimMessage
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ Auth s

versionParser :: Int -> Parser VimMessage
versionParser bufId = do
    string "version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ EventMessage bufId seqN $ Version ver

startupDoneParser :: Int -> Parser VimMessage
startupDoneParser bufId = do
    string "startupDone="
    seqN <- parseNumber
    return $ EventMessage bufId seqN $ StartupDone

fileOpenedParser :: Int -> Parser VimMessage
fileOpenedParser bufId = do
    string "fileOpened="
    seqN <- parseNumber
    string " \""
    path <- many1 $ noneOf "\""
    string "\" "
    open <- oneOf "TF"
    char ' '
    modified <- oneOf "TF"
    return $ EventMessage bufId seqN $ FileOpened path (open == 'T') (modified == 'T')

keyCommandParser :: Int -> Parser VimMessage
keyCommandParser bufId = do
    string "keyCommand="
    seqN <- parseNumber
    string " \""
    key <- many1 $ noneOf "\""
    return $ EventMessage bufId seqN $ KeyCommand key

newDotAndMarkParser :: Int -> Parser VimMessage
newDotAndMarkParser bufId = do
    string "newDotAndMark="
    seqNo <- parseNumber
    char ' '
    off1 <- parseNumber
    char ' '
    off2 <- parseNumber
    return $ EventMessage bufId seqNo $ NewDotAndMark off1 off2

parseMessage :: ParserMap -> String -> Either String VimMessage
parseMessage parserMap m = case parse (messageParser parserMap) "(unknown)" m of
    Left parseError -> Left $ show parseError
    Right message -> Right message

printMessage :: IdeMessage -> String
printMessage DisconnectCommand = "DISCONNECT\n"
printMessage Detach = "DETACH\n"
printMessage (CommandMessage bufId seqNo command) =
    (show bufId) ++ ":" ++ (printCommandName command) ++ "!" ++ (show seqNo)
    ++ (printCommandArgs command)
printMessage (FunctionMessage bufId seqNo function) =
    (show bufId) ++ ":" ++ (printFunctionName function) ++ "/" ++ (show seqNo)
    ++ (printFunctionArgs function)

printBool :: Bool -> String
printBool b = case b of
    True -> "T"
    False -> "F"

printCommandName :: Command -> String
printCommandName AddAnno {} = "addAnno"
printCommandName Close {} = "close"
printCommandName Create = "create"
printCommandName DefineAnnoType {} = "defineAnnoType"
printCommandName EditFile {} = "editFile"
printCommandName EndAtomic {} = "endAtomic"
printCommandName Guard {} = "guard"
printCommandName InitDone {} = "initDone"
printCommandName InsertDone {} = "insertDone"
printCommandName NetbeansBuffer {} = "netbeansBuffer"
printCommandName PutBufferNumber {} = "putBufferNumber"
printCommandName Raise {} = "raise"
printCommandName RemoveAnno {} = "removeAnno"
printCommandName Save {} = "save"
printCommandName SaveDone {} = "saveDone"
printCommandName SetBufferNumber {} = "setBufferNumber"
printCommandName SetDot {} = "setDot"
printCommandName SetExitDelay {} = "setExitDelay"
printCommandName SetFullName {} = "setFullName"
printCommandName SetModified {} = "setModified"
printCommandName SetReadOnly {} = "setReadOnly"
printCommandName SetTitle {} = "setTitle"
printCommandName SetVisible {} = "setVisible"
printCommandName ShowBalloon {} = "showBalloon"
printCommandName SpecialKeys {} = "specialKeys"
printCommandName StartAtomic {} = "startAtomic"
printCommandName StartDocumentListen {} = "startDocumentListen"
printCommandName StopDocumentListen {} = "stopDocumentListen"
printCommandName Unguard {} = "unguard"

printCommandArgs :: Command -> String
printCommandArgs (AddAnno serNum typeNum off len) =
       " " ++ (show serNum)
    ++ " " ++ (show typeNum)
    ++ " " ++ (show off)
    ++ " " ++ (show len)
printCommandArgs Close = ""
printCommandArgs Create = ""
printCommandArgs (DefineAnnoType typeNum typeName tooltip glyphFile fg bg) =
    " " ++ (show typeNum)
    ++ " " ++ (show typeName) ++ ""
    ++ " " ++ (show tooltip) ++ ""
    ++ " " ++ (show glyphFile) ++ ""
    ++ " " ++ (show fg)
    ++ " " ++ (show bg)
printCommandArgs (EditFile path) =
       " " ++ (show path) ++ ""
printCommandArgs EndAtomic = ""
printCommandArgs (Guard off len) =
       " " ++ (show off)
    ++ " " ++ (show len)
printCommandArgs InitDone = ""
printCommandArgs InsertDone = ""
printCommandArgs (NetbeansBuffer isNetbeansBuffer) =
       " " ++ (printBool isNetbeansBuffer)
printCommandArgs (PutBufferNumber path) =
       " " ++ (show path)
printCommandArgs Raise = ""
printCommandArgs (RemoveAnno serNum) =
       " " ++ (show serNum)
printCommandArgs Save = ""
printCommandArgs SaveDone = ""
printCommandArgs (SetBufferNumber path) =
       " " ++ (show path)
printCommandArgs (SetDot off) =
       " " ++ (show off)
printCommandArgs (SetExitDelay delay) =
       " " ++ (show delay)
printCommandArgs (SetFullName fullName) =
       " " ++ (show fullName)
printCommandArgs (SetModified modified) =
       " " ++ (printBool modified)
printCommandArgs SetReadOnly = ""
printCommandArgs (SetTitle title) =
       " " ++ (show title)
printCommandArgs (SetVisible visible) =
       " " ++ (printBool visible)
printCommandArgs (ShowBalloon text) =
       " " ++ (show text)
printCommandArgs SpecialKeys = ""
printCommandArgs StartAtomic = ""
printCommandArgs StartDocumentListen = ""
printCommandArgs StopDocumentListen = ""
printCommandArgs (Unguard off len) =
       " " ++ (show off)
    ++ " " ++ (show len)

printFunctionName :: Function -> String
printFunctionName GetCursor = "getCursor"
printFunctionName GetLength = "getLength"
printFunctionName GetAnno {} = "getAnno"
printFunctionName GetModified {} = "getModified"
printFunctionName GetText {} = "getText"
printFunctionName Insert {} = "insert"
printFunctionName Remove {} = "remove"
printFunctionName SaveAndExit {} = "saveAndExit"

printFunctionArgs :: Function -> String
printFunctionArgs GetCursor = ""
printFunctionArgs GetLength = ""
printFunctionArgs (GetAnno serNum) =
       " " ++ (show serNum)
printFunctionArgs GetModified = ""
printFunctionArgs GetText = ""
printFunctionArgs (Insert off text) =
       " " ++ (show off)
    ++ " " ++ (show text)
printFunctionArgs (Remove off len) =
       " " ++ (show off)
    ++ " " ++ (show len)
printFunctionArgs SaveAndExit = ""
