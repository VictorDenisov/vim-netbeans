import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (runNetbeans, getLength, getCursor, getAnno)
import Control.Monad.Trans (liftIO)
import Network

main = runNetbeans (PortNumber 4444) "password" $ do
    l <- getLength 0
    x <- getCursor
    y <- getAnno 0 1
    liftIO $ putStrLn $ show l
    liftIO $ putStrLn $ show x
    liftIO $ putStrLn $ show y
