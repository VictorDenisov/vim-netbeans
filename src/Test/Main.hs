import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (runNetbeans, getLength, getCursor, getAnno, editFile, nextEvent)
import Control.Monad.Trans (liftIO)
import Network

main = runNetbeans (PortNumber 4444) "password" $ do
    b <- editFile "README"
    l <- getLength b
    liftIO $ putStrLn $ show b
    liftIO $ putStrLn $ show l
    e <- nextEvent
    liftIO $ putStrLn $ show e
