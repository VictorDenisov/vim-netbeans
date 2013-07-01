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

printBool :: Bool -> String
printBool b = case b of
    True -> "T"
    False -> "F"

printMessage :: IdeMessage -> String
printMessage DisconnectCommand = "DISCONNECT\n"
printMessage Detach = "DETACH\n"
printMessage (CommandMessage bufId seqNo Create) = (show bufId) ++ ":create!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (EditFile path)) =
    (show bufId) ++ ":editFile!" ++ (show seqNo) ++
    " \"" ++ path ++ "\""
printMessage (CommandMessage bufId seqNo EndAtomic) =
    (show bufId) ++ ":endAtomic!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (Guard off len)) =
    (show bufId) ++ ":guard!" ++ (show seqNo)
    ++ " " ++ (show off)
    ++ " " ++ (show len)
printMessage (CommandMessage bufId seqNo SetReadOnly) =
    (show bufId) ++ ":setReadOnly!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (AddAnno serNum typeNum off len)) =
    (show bufId) ++ ":addAnno!" ++ (show seqNo)
    ++ " " ++ (show serNum)
    ++ " " ++ (show typeNum)
    ++ " " ++ (show off)
    ++ " " ++ (show len)
printMessage (CommandMessage bufId seqNo Close) =
    (show bufId) ++ ":close!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo InitDone) =
    (show bufId) ++ ":initDone!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo InsertDone) =
    (show bufId) ++ ":insertDone!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (NetbeansBuffer isNetbeans)) =
    (show bufId) ++ ":netbeansBuffer!" ++ (show seqNo)
    ++ " " ++ (printBool isNetbeans)
printMessage (CommandMessage bufId seqNo (PutBufferNumber path)) =
    (show bufId) ++ ":putBufferNumber!" ++ (show seqNo)
    ++ " " ++ (show path)
printMessage (CommandMessage bufId seqNo Raise) =
    (show bufId) ++ ":raise!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (RemoveAnno serNum)) =
    (show bufId) ++ ":removeAnno!" ++ (show seqNo)
    ++ " " ++ (show serNum)
printMessage (CommandMessage bufId seqNo Save) =
    (show bufId) ++ ":save!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo SaveDone) =
    (show bufId) ++ ":saveDone!" ++ (show seqNo)
printMessage (CommandMessage bufId seqNo (DefineAnnoType typeNum typeName tooltip glyphFile fg bg)) =
    (show bufId) ++ ":defineAnnoType!" ++ (show seqNo)
    ++ " " ++ (show typeNum)
    ++ " \"" ++ typeName ++ "\""
    ++ " \"" ++ tooltip ++ "\""
    ++ " \"" ++ glyphFile ++ "\""
    ++ " " ++ (show fg)
    ++ " " ++ (show bg)
