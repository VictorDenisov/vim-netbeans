import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (runNetbeans, getLength, getCursor)
import Control.Monad.Trans (liftIO)
import Network

main = runNetbeans (PortNumber 4444) "password" $ do
    l <- getLength 0
    x <- getCursor
    liftIO $ putStrLn $ show l
    liftIO $ putStrLn $ show x
