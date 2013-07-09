import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans (Netbeans, runNetbeans, getLength, getCursor, getAnno, editFile, nextEvent, tryNextEvent, defineAnnoType)
import Vim.Netbeans.Protocol (Color(..))
import Control.Monad.Trans (liftIO)
import Network
import Control.Monad.IO.Class (MonadIO)

main = runNetbeans (PortNumber 4444) "password" $ do
    b <- editFile "README"
    l <- getLength b
    a <- defineAnnoType b "myTypeName" "" "=>" Red Blue
    liftIO $ putStrLn $ show a
    liftIO $ putStrLn $ show b
    liftIO $ putStrLn $ show l
    pollAllEvents

pollAllEvents :: MonadIO m => Netbeans m ()
pollAllEvents = do
    ne <- tryNextEvent
    case ne of
        Nothing -> return ()
        Just e -> do
            liftIO $ putStrLn $ show e
            pollAllEvents
