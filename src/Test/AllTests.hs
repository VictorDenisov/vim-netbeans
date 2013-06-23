module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (N.Auth "password") @=? (N.parseMessage "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (N.Version 0 1 "2.5") @=? (N.parseMessage "0:version=1 \"2.5\"")

parseStartupDoneMessage :: Assertion
parseStartupDoneMessage = (N.StartupDone 0 1) @=? (N.parseMessage "0:startupDone=1")

parseErrorMessage :: Assertion
parseErrorMessage = N.E463 @=? (N.parseMessage "E463")

printAuthMessage :: Assertion
printAuthMessage = "AUTH password\n" @=? (N.printMessage (N.Auth "password"))

printCreateMessage :: Assertion
printCreateMessage = "0:create!1" @=? (N.printMessage $ N.Create 0 1)

printDisconnectMessage :: Assertion
printDisconnectMessage = "DISCONNECT\n" @=? (N.printMessage N.Disconnect)

printDetachMessage :: Assertion
printDetachMessage = "DETACH\n" @=? (N.printMessage N.Detach)
