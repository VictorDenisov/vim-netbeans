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

data Message = Auth String
             | Disconnect
             | Detach
             | Version
                Int -- buf
                Int -- seqNo
                String
             | ErroneousMessage String
               deriving (Eq, Show)

parseNumber :: CharParser st Int
parseNumber = read <$> many1 digit

messageParser :: CharParser st Message
messageParser = try (authParser) <|> versionParser

authParser :: CharParser st Message
authParser = do
    string "AUTH "
    s <- many1 anyChar
    return $ Auth s

versionParser :: CharParser st Message
versionParser = do
    bufId <- parseNumber
    string ":version="
    seqN <- parseNumber
    string " \""
    ver <- many1 (oneOf "0123456789.")
    char '\"'
    return $ Version bufId seqN ver

parseMessage :: String -> Message
parseMessage m = case parse messageParser "(unknown)" m of
    Left parseError -> ErroneousMessage $ show parseError
    Right message -> message

printMessage :: Message -> String
printMessage (Auth s) = "AUTH " ++ s ++ "\n"
printMessage Disconnect = "DISCONNECT\n"
printMessage Detach = "DETACH\n"
