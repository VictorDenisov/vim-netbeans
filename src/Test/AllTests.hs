module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (Right $ N.Auth "password") @=? (N.parseMessage "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (Right $ N.Version 0 1 "2.5") @=? (N.parseMessage "0:version=1 \"2.5\"")

parseFileOpenedMessage :: Assertion
parseFileOpenedMessage = (Right $ N.FileOpened 0 1 "pathname" True False) @=? (N.parseMessage "0:fileOpened=1 \"pathname\" T F")

parseKeyCommandMessage :: Assertion
parseKeyCommandMessage = (Right $ N.KeyCommand 0 1 "F1")
                         @=?
                         (N.parseMessage "0:keyCommand=1 \"F1\"")

parseNewDotAndMarkMessage :: Assertion
parseNewDotAndMarkMessage = (Right $ N.NewDotAndMark 0 1 3 4) @=? (N.parseMessage "0:newDotAndMark=1 3 4")

parseStartupDoneMessage :: Assertion
parseStartupDoneMessage = (Right $ N.StartupDone 0 1) @=? (N.parseMessage "0:startupDone=1")

parseErrorMessage :: Assertion
parseErrorMessage = (Right N.E463) @=? (N.parseMessage "E463")

printAuthMessage :: Assertion
printAuthMessage = "AUTH password\n" @=? (N.printMessage (N.Auth "password"))

printCreateMessage :: Assertion
printCreateMessage = "0:create!1" @=? (N.printMessage $ N.Create 0 1)

printDisconnectMessage :: Assertion
printDisconnectMessage = "DISCONNECT\n" @=? (N.printMessage N.Disconnect)

printDetachMessage :: Assertion
printDetachMessage = "DETACH\n" @=? (N.printMessage N.Detach)
