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
           | KeyCommand
                String -- key
           | NewDotAndMark
                Int -- off1
                Int -- off2
           | StartupDone
           | Version
                String
           | Auth String
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

type BufId = Int
type AnnoTypeNum = Int
type AnnoNum = Int

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
                      <|> try parseError
                      <|> try (regularMessageParser parserMap)

regularMessageParser :: ParserMap -> Parser VimMessage
regularMessageParser parserMap = do
    num <- parseNumber
    try (eventParser num) <|> replyParser num parserMap

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
    return $ GetCursorReply bufId lnum col off

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

saveAndExitReplyParser :: Parser Reply
saveAndExitReplyParser = undefined -- This parser is place holder only.
                                   -- It should not be invoked ever.

eventParser :: BufId -> Parser VimMessage
eventParser bufId = char ':' >>
                   (try (balloonTextParser bufId)
                <|> try (buttonReleaseParser bufId)
                <|> try (disconnectParser bufId)
                <|> try (fileOpenedParser bufId)
                <|> try (keyCommandParser bufId)
                <|> try (newDotAndMarkParser bufId)
                <|> try (startupDoneParser bufId)
                <|> try (versionParser bufId))

parseError :: Parser VimMessage
parseError = do
    char 'E'
    s <- count 3 digit
    case s of
        "463" -> return $ EventMessage (-1) (-1) $ E463 -- Region is guarded, cannot modify
        "532" -> return $ EventMessage (-1) (-1) $ E532 -- The defineAnnoType highlighting color name is too long
        "656" -> return $ EventMessage (-1) (-1) $ E656 -- Writes of unmodified buffers forbidden
        "657" -> return $ EventMessage (-1) (-1) $ E657 -- Partial writes disallowed
        "658" -> return $ EventMessage (-1) (-1) $ E658 -- Connection lost for this buffer
        "744" -> return $ EventMessage (-1) (-1) $ E744 -- Read-only file

authParser :: Parser VimMessage
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ EventMessage (-1) (-1) $ Auth s

versionParser :: BufId -> Parser VimMessage
versionParser bufId = do
    string "version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ EventMessage bufId seqN $ Version ver

startupDoneParser :: BufId -> Parser VimMessage
startupDoneParser bufId = do
    string "startupDone="
    seqN <- parseNumber
    return $ EventMessage bufId seqN $ StartupDone

parseString :: Parser String
parseString = do
    string "\""
    s <- many1 $ noneOf "\""
    string "\""
    return s

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
    open <- oneOf "TF"
    char ' '
    modified <- oneOf "TF"
    return $ EventMessage bufId seqN $ FileOpened path (open == 'T') (modified == 'T')

keyCommandParser :: BufId -> Parser VimMessage
keyCommandParser bufId = do
    string "keyCommand="
    seqN <- parseNumber
    char ' '
    key <- parseString
    return $ EventMessage bufId seqN $ KeyCommand key

newDotAndMarkParser :: BufId -> Parser VimMessage
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
