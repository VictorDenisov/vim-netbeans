import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (runNetbeans)
import Network

main = runNetbeans (PortNumber 4444) "password" (return ())
