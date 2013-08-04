module Vim.Netbeans.Protocol
where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

data VimMessage = EventMessage BufId Int Event -- buf seqNo event
                | ReplyMessage Int Reply -- seqNo
                  deriving (Eq, Show)

data IdeMessage = CommandMessage BufId Int Command -- buf seqNo command
                | FunctionMessage BufId Int Function -- buf seqNo function
                | Detach
                | DisconnectCommand
                  deriving (Eq, Show)

data Reply = GetCursorReply
                BufId -- bufId
                Int -- lnum
                Int -- col
                Int -- off
           | GetLengthReply
                Int -- len
           | GetAnnoReply
                Int -- lnum
           | GetModifiedReply
                Int -- modified count or boolean
           | GetTextReply
                String -- text
           | InsertReplySuccess
           | InsertReplyError
                String -- error message
           | RemoveReplySuccess
           | RemoveReplyError
                String -- error message
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

data Event = BalloonText
                String -- text
           | ButtonRelease
                Int -- button
                Int -- lnum
                Int -- col
           | Disconnect
           | FileOpened
                String -- path
                Bool -- open
                Bool -- modified
           | Geometry
                Int -- cols
                Int -- rows
                Int -- x
                Int -- y
           | InsertEvent
                Int -- off
                String -- text
           | KeyCommand
                String -- key
           | KeyAtPos
                String -- key
                Int -- off
                Int -- lnum
                Int -- col
           | Killed
           | NewDotAndMark
                Int -- off1
                Int -- off2
           | RemoveEvent
                Int -- off
                Int -- len
           | SaveEvent
           | StartupDone
           | Unmodified
           | Version
                String
           | Auth String
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
                AnnoNum -- serNum
                AnnoTypeNum -- typeNum
                Int -- off
                Int -- len
             | Close
             | Create
             | DefineAnnoType
                AnnoTypeNum -- typeNum
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
                AnnoNum -- serNum
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

newtype BufId = BufId Int

instance Show BufId where
    show (BufId x) = show x

instance Eq BufId where
    (BufId x) == (BufId y) = x == y

newtype AnnoTypeNum = AnnoTypeNum Int

instance Show AnnoTypeNum where
    show (AnnoTypeNum v) = show v

instance Eq AnnoTypeNum where
    (AnnoTypeNum x) == (AnnoTypeNum y) = x == y

newtype AnnoNum = AnnoNum Int

instance Show AnnoNum where
    show (AnnoNum v) = show v

instance Eq AnnoNum where
    (AnnoNum x) == (AnnoNum y) = x == y

type ParserMap = [(Int, Parser Reply)]

parseNumber :: Parser Int
parseNumber = do
    sign <- optionMaybe $ string "-"
    v <- read <$> many1 digit
    case sign of
        Nothing -> return v
        Just _ -> return (-v)

messageParser :: ParserMap -> Parser VimMessage
messageParser parserMap = try authParser
                      <|> try (regularMessageParser parserMap)

regularMessageParser :: ParserMap -> Parser VimMessage
regularMessageParser parserMap = do
    num <- parseNumber
    try (eventParser (BufId num)) <|> replyParser num parserMap

replyParser :: Int -> ParserMap -> Parser VimMessage
replyParser seqno parserMap = do
    let parser = fromJust $ lookup seqno parserMap
    reply <- parser
    return $ ReplyMessage seqno reply

getCursorReplyParser :: Parser Reply
getCursorReplyParser = do
    char ' '
    bufId <- parseNumber
    char ' '
    lnum <- parseNumber
    char ' '
    col <- parseNumber
    char ' '
    off <- parseNumber
    return $ GetCursorReply (BufId bufId) lnum col off

getLengthReplyParser :: Parser Reply
getLengthReplyParser = do
    char ' '
    len <- parseNumber
    return $ GetLengthReply len

getAnnoReplyParser :: Parser Reply
getAnnoReplyParser = do
    char ' '
    lnum <- parseNumber
    return $ GetAnnoReply lnum

getModifiedReplyParser :: Parser Reply
getModifiedReplyParser = do
    char ' '
    count <- parseNumber
    return $ GetModifiedReply count

getTextReplyParser :: Parser Reply
getTextReplyParser = do
    char ' '
    text <- parseString
    return $ GetTextReply text

insertReplyParser :: Parser Reply
insertReplyParser = do
    try insertErrorReplyParser <|> return InsertReplySuccess

insertErrorReplyParser :: Parser Reply
insertErrorReplyParser = do
    string " !"
    text <- many1 $ anyChar
    return $ InsertReplyError text

removeReplyParser :: Parser Reply
removeReplyParser = do
    try removeErrorReplyParser <|> return RemoveReplySuccess

removeErrorReplyParser :: Parser Reply
removeErrorReplyParser = do
    string " !"
    text <- many1 $ anyChar
    return $ RemoveReplyError text

eventParser :: BufId -> Parser VimMessage
eventParser bufId = char ':' >>
                   (try (balloonTextParser bufId)
                <|> try (buttonReleaseParser bufId)
                <|> try (disconnectParser bufId)
                <|> try (fileOpenedParser bufId)
                <|> try (geometryParser bufId)
                <|> try (insertParser bufId)
                <|> try (keyCommandParser bufId)
                <|> try (keyAtPosParser bufId)
                <|> try (killedParser bufId)
                <|> try (newDotAndMarkParser bufId)
                <|> try (removeParser bufId)
                <|> try (saveParser bufId)
                <|> try (startupDoneParser bufId)
                <|> try (unmodifiedParser bufId)
                <|> try (versionParser bufId))

unescapedQuote :: Parser Char
unescapedQuote = do
    c <- noneOf "\""
    if c == '\\'
        then do
            nc <- anyChar
            case nc of
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                x -> return x
        else
            return c


parseString :: Parser String
parseString = do
    string "\""
    s <- many1 unescapedQuote
    string "\""
    return s

parseBool :: Parser Bool
parseBool = do
    value <- oneOf "TF"
    return $ value == 'T'

authParser :: Parser VimMessage
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ EventMessage (BufId (-1)) (-1) $ Auth s

balloonTextParser :: BufId -> Parser VimMessage
balloonTextParser bufId = do
    string "balloonText="
    seqNo <- parseNumber
    char ' '
    text <- parseString
    return $ EventMessage bufId seqNo $ BalloonText text

buttonReleaseParser :: BufId -> Parser VimMessage
buttonReleaseParser bufId = do
    string "buttonRelease="
    seqNo <- parseNumber
    char ' '
    button <- parseNumber
    char ' '
    lnum <- parseNumber
    char ' '
    col <- parseNumber
    return $ EventMessage bufId seqNo $ ButtonRelease button lnum col

disconnectParser :: BufId -> Parser VimMessage
disconnectParser bufId = do
    string "disconnect="
    seqNo <- parseNumber
    return $ EventMessage bufId seqNo $ Disconnect

fileOpenedParser :: BufId -> Parser VimMessage
fileOpenedParser bufId = do
    string "fileOpened="
    seqN <- parseNumber
    char ' '
    path <- parseString
    char ' '
    open <- parseBool
    char ' '
    modified <- parseBool
    return $ EventMessage bufId seqN $ FileOpened path open modified

geometryParser :: BufId -> Parser VimMessage
geometryParser bufId = do
    string "geometry="
    seqN <- parseNumber
    char ' '
    cols <- parseNumber
    char ' '
    rows <- parseNumber
    char ' '
    x <- parseNumber
    char ' '
    y <- parseNumber
    return $ EventMessage bufId seqN $ Geometry cols rows x y

insertParser :: BufId -> Parser VimMessage
insertParser bufId = do
    string "insert="
    seqN <- parseNumber
    char ' '
    off <- parseNumber
    char ' '
    text <- parseString
    return $ EventMessage bufId seqN $ InsertEvent off text

keyCommandParser :: BufId -> Parser VimMessage
keyCommandParser bufId = do
    string "keyCommand="
    seqN <- parseNumber
    char ' '
    key <- parseString
    return $ EventMessage bufId seqN $ KeyCommand key

keyAtPosParser :: BufId -> Parser VimMessage
keyAtPosParser bufId = do
    string "keyAtPos="
    seqN <- parseNumber
    char ' '
    key <- parseString
    char ' '
    off <- parseNumber
    char ' '
    lnum <- parseNumber
    char '/'
    col <- parseNumber
    return $ EventMessage bufId seqN $ KeyAtPos key off lnum col

killedParser :: BufId -> Parser VimMessage
killedParser bufId = do
    string "killed="
    seqN <- parseNumber
    return $ EventMessage bufId seqN $ Killed

newDotAndMarkParser :: BufId -> Parser VimMessage
newDotAndMarkParser bufId = do
    string "newDotAndMark="
    seqNo <- parseNumber
    char ' '
    off1 <- parseNumber
    char ' '
    off2 <- parseNumber
    return $ EventMessage bufId seqNo $ NewDotAndMark off1 off2

removeParser :: BufId -> Parser VimMessage
removeParser bufId = do
    string "remove="
    seqNo <- parseNumber
    char ' '
    off <- parseNumber
    char ' '
    len <- parseNumber
    return $ EventMessage bufId seqNo $ RemoveEvent off len

saveParser :: BufId -> Parser VimMessage
saveParser bufId = do
    string "save="
    seqNo <- parseNumber
    return $ EventMessage bufId seqNo $ SaveEvent

startupDoneParser :: BufId -> Parser VimMessage
startupDoneParser bufId = do
    string "startupDone="
    seqN <- parseNumber
    return $ EventMessage bufId seqN $ StartupDone

unmodifiedParser :: BufId -> Parser VimMessage
unmodifiedParser bufId = do
    string "unmodified="
    seqNo <- parseNumber
    return $ EventMessage bufId seqNo $ Unmodified

versionParser :: BufId -> Parser VimMessage
versionParser bufId = do
    string "version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ EventMessage bufId seqN $ Version ver

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
