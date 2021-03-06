import Network.Socket.Internal (PortNumber(..))
import Vim.Netbeans
import Control.Monad.Trans (liftIO)
import Network
import Control.Monad.IO.Class (MonadIO)

main = runNetbeans
            (PortNumber 4444)
            "password"
            (NetbeansCallbacks
                (liftIO $ putStrLn "Waiting for connection")
                (\e -> case e of
                    StartupDone -> liftIO $ putStrLn "Startup done"
                    _ -> liftIO $ putStrLn "other event"
                )
            ) $ do
    b <- editFile "ttt"
    l <- getLength b
    a <- defineAnnoType b "myTypeName" "" "=>" Red Blue
    an <- addAnno b a 1
    liftIO $ putStrLn $ show a
    liftIO $ putStrLn $ show b
    liftIO $ putStrLn $ show l
    startAtomic b
    insert b 1 "############"
    insert b 2 "---------------"
    liftIO $ putStrLn "Before endAtomic"
    liftIO $ getLine
    endAtomic b
    liftIO $ putStrLn "After endAtomic"
    liftIO $ getLine
    setNetbeansBuffer b
    liftIO $ putStrLn "After setNetbeans true"
    liftIO $ getLine
    clearNetbeansBuffer b
    liftIO $ putStrLn "After setNetbeans false"
    liftIO $ getLine
    saveAndExit
    pollAllEvents

pollAllEvents :: MonadIO m => Netbeans m ()
pollAllEvents = do
    ne <- nextEvent
    case ne of
        (_, Disconnect) -> return ()
        (_, e) -> do
            liftIO $ putStrLn $ "printing event" ++ (show e)
            pollAllEvents
