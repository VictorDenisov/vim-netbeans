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
    let message = parseMessage line
    liftIO $ putStrLn $ show message
    runStateT vm (initialConnState handleC)
    return ()

initialConnState :: Handle -> ConnState
initialConnState h = ConnState 0 h Nothing

data VimMessage = EventMessage Int Int Event -- buf seqNo event
                | ReplyMessage Reply
                | Auth String
                | E463
                | E532
                | E656
                | E657
                | E658
                | E744
                  deriving (Eq, Show)

data IdeMessage = CommandMessage Int Int Command -- buf seqNo command
                | FunctionMessage Function
                | Detach
                | DisconnectCommand
                  deriving (Eq, Show)

data Reply = Reply
             deriving (Eq, Show)
data Function = Function
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
               deriving (Eq, Show)

parseNumber :: CharParser st Int
parseNumber = read <$> many1 digit

messageParser :: CharParser st VimMessage
messageParser = try authParser
            <|> try versionParser
            <|> try startupDoneParser
            <|> try fileOpenedParser
            <|> try newDotAndMarkParser
            <|> try keyCommandParser
            <|> parseError

parseError :: CharParser st VimMessage
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

authParser :: CharParser st VimMessage
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ Auth s

versionParser :: CharParser st VimMessage
versionParser = do
    bufId <- parseNumber
    string ":version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ EventMessage bufId seqN $ Version ver

startupDoneParser :: CharParser st VimMessage
startupDoneParser = do
    bufId <- parseNumber
    string ":startupDone="
    seqN <- parseNumber
    return $ EventMessage bufId seqN $ StartupDone

fileOpenedParser :: CharParser st VimMessage
fileOpenedParser = do
    bufId <- parseNumber
    string ":fileOpened="
    seqN <- parseNumber
    string " \""
    path <- many1 $ noneOf "\""
    string "\" "
    open <- oneOf "TF"
    char ' '
    modified <- oneOf "TF"
    return $ EventMessage bufId seqN $ FileOpened path (open == 'T') (modified == 'T')

keyCommandParser :: CharParser st VimMessage
keyCommandParser = do
    bufId <- parseNumber
    string ":keyCommand="
    seqN <- parseNumber
    string " \""
    key <- many1 $ noneOf "\""
    return $ EventMessage bufId seqN $ KeyCommand key

newDotAndMarkParser :: CharParser st VimMessage
newDotAndMarkParser = do
    bufId <- parseNumber
    string ":newDotAndMark="
    seqNo <- parseNumber
    char ' '
    off1 <- parseNumber
    char ' '
    off2 <- parseNumber
    return $ EventMessage bufId seqNo $ NewDotAndMark off1 off2

parseMessage :: String -> Either String VimMessage
parseMessage m = case parse messageParser "(unknown)" m of
    Left parseError -> Left $ show parseError
    Right message -> Right message

printMessage :: IdeMessage -> String
printMessage DisconnectCommand = "DISCONNECT\n"
printMessage Detach = "DETACH\n"
printMessage (CommandMessage bufId seqNo command) =
    (show bufId) ++ ":" ++ (printCommandName command) ++ "!" ++ (show seqNo)
    ++ (printCommandArgs command)

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
