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

data VimMessage = EventMessage Event
                | ReplyMessage Reply
                  deriving (Eq, Show)

data IdeMessage = CommandMessage  Command
                | FunctionMessage Function
                  deriving (Eq, Show)

data Reply = Reply
             deriving (Eq, Show)
data Function = Function
                deriving (Eq, Show)

data Event = Auth String
           | FileOpened
                Int -- buf
                Int -- seqNo
                String -- path
                Bool -- open
                Bool -- modified
           | KeyCommand
                Int -- buf
                Int -- seqNo
                String -- key
           | NewDotAndMark
                Int -- buf
                Int -- seqNo
                Int -- off1
                Int -- off2
           | StartupDone
                Int -- buf
                Int -- seqNo
           | Version
                Int -- buf
                Int -- seqNo
                String
           | E463
           | E532
           | E656
           | E657
           | E658
           | E744
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
                Int -- buf
                Int -- seqNo
                Int -- serNum
                Int -- typeNum
                Int -- off
                Int -- len
             | Close
                Int -- buf
                Int -- seqNo
             | Create
                Int -- buf
                Int -- seqNo
             | DefineAnnoType
                Int -- buf
                Int -- seqNo
                Int -- typeNum
                String -- typeName
                String -- toolTip
                String -- glyphFile
                Color -- fg
                Color -- bg
             | Detach
             | DisconnectCommand
             | EditFile
                Int -- buf
                Int -- seqNo
                String -- path
             | EndAtomic
                Int -- buf
                Int -- seqNo
             | Guard
                Int -- buf
                Int -- seqNo
                Int -- off
                Int -- len
             | InitDone
                Int -- buf
                Int -- seqNo
             | InsertDone
                Int -- buf
                Int -- seqNo
             | NetbeansBuffer
                Int -- buf
                Int -- seqNo
                Bool -- isNetbeansBuffer
             | PutBufferNumber
                Int -- buf
                Int -- seqNo
                String -- isNetbeansBuffer
             | Raise
                Int -- buf
                Int -- seqNo
             | SetReadOnly
                Int -- buf
                Int -- seqNo
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
        "463" -> return $ EventMessage E463 -- Region is guarded, cannot modify
        "532" -> return $ EventMessage E532 -- The defineAnnoType highlighting color name is too long
        "656" -> return $ EventMessage E656 -- Writes of unmodified buffers forbidden
        "657" -> return $ EventMessage E657 -- Partial writes disallowed
        "658" -> return $ EventMessage E658 -- Connection lost for this buffer
        "744" -> return $ EventMessage E744 -- Read-only file

authParser :: CharParser st VimMessage
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ EventMessage $ Auth s

versionParser :: CharParser st VimMessage
versionParser = do
    bufId <- parseNumber
    string ":version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ EventMessage $ Version bufId seqN ver

startupDoneParser :: CharParser st VimMessage
startupDoneParser = do
    bufId <- parseNumber
    string ":startupDone="
    seqN <- parseNumber
    return $ EventMessage $ StartupDone bufId seqN

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
    return $ EventMessage $ FileOpened bufId seqN path (open == 'T') (modified == 'T')

keyCommandParser :: CharParser st VimMessage
keyCommandParser = do
    bufId <- parseNumber
    string ":keyCommand="
    seqN <- parseNumber
    string " \""
    key <- many1 $ noneOf "\""
    return $ EventMessage $ KeyCommand bufId seqN key

newDotAndMarkParser :: CharParser st VimMessage
newDotAndMarkParser = do
    bufId <- parseNumber
    string ":newDotAndMark="
    seqNo <- parseNumber
    char ' '
    off1 <- parseNumber
    char ' '
    off2 <- parseNumber
    return $ EventMessage $ NewDotAndMark bufId seqNo off1 off2

parseMessage :: String -> Either String VimMessage
parseMessage m = case parse messageParser "(unknown)" m of
    Left parseError -> Left $ show parseError
    Right message -> Right message

printBool :: Bool -> String
printBool b = case b of
    True -> "T"
    False -> "F"

printMessage :: IdeMessage -> String
printMessage (CommandMessage DisconnectCommand) = "DISCONNECT\n"
printMessage (CommandMessage Detach) = "DETACH\n"
printMessage (CommandMessage (Create bufId seqNo)) = (show bufId) ++ ":create!" ++ (show seqNo)
printMessage (CommandMessage (EditFile bufId seqNo path)) =
    (show bufId) ++ ":editFile!" ++ (show seqNo) ++
    " \"" ++ path ++ "\""
printMessage (CommandMessage (EndAtomic bufId seqNo)) =
    (show bufId) ++ ":endAtomic!" ++ (show seqNo)
printMessage (CommandMessage (Guard bufId seqNo off len)) =
    (show bufId) ++ ":guard!" ++ (show seqNo)
    ++ " " ++ (show off)
    ++ " " ++ (show len)
printMessage (CommandMessage (SetReadOnly bufId seqNo)) =
    (show bufId) ++ ":setReadOnly!" ++ (show seqNo)
printMessage (CommandMessage (AddAnno bufId seqNo serNum typeNum off len)) =
    (show bufId) ++ ":addAnno!" ++ (show seqNo)
    ++ " " ++ (show serNum)
    ++ " " ++ (show typeNum)
    ++ " " ++ (show off)
    ++ " " ++ (show len)
printMessage (CommandMessage (Close bufId seqNo)) =
    (show bufId) ++ ":close!" ++ (show seqNo)
printMessage (CommandMessage (InitDone bufId seqNo)) =
    (show bufId) ++ ":initDone!" ++ (show seqNo)
printMessage (CommandMessage (InsertDone bufId seqNo)) =
    (show bufId) ++ ":insertDone!" ++ (show seqNo)
printMessage (CommandMessage (NetbeansBuffer bufId seqNo isNetbeans)) =
    (show bufId) ++ ":netbeansBuffer!" ++ (show seqNo)
    ++ " " ++ (printBool isNetbeans)
printMessage (CommandMessage (PutBufferNumber bufId seqNo path)) =
    (show bufId) ++ ":putBufferNumber!" ++ (show seqNo)
    ++ " " ++ (show path)
printMessage (CommandMessage (Raise bufId seqNo)) =
    (show bufId) ++ ":raise!" ++ (show seqNo)
printMessage (CommandMessage (DefineAnnoType bufId seqNo typeNum typeName tooltip glyphFile fg bg)) =
    (show bufId) ++ ":defineAnnoType!" ++ (show seqNo)
    ++ " " ++ (show typeNum)
    ++ " \"" ++ typeName ++ "\""
    ++ " \"" ++ tooltip ++ "\""
    ++ " \"" ++ glyphFile ++ "\""
    ++ " " ++ (show fg)
    ++ " " ++ (show bg)
