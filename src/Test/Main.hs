import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (runNetbeans, getLength)
import Control.Monad.Trans (liftIO)
import Network

main = runNetbeans (PortNumber 4444) "password" $ do
    l <- getLength 0
    liftIO $ putStrLn $ show l
