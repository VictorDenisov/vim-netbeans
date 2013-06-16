module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (N.Auth "password") @=? (N.parseMessage "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (N.Version 0 1 "2.5") @=? (N.parseMessage "0:version=1 \"2.5\"")

printAuthMessage :: Assertion
printAuthMessage = ("AUTH password\n") @=? (N.printMessage (N.Auth "password"))

printDisconnectMessage :: Assertion
printDisconnectMessage = "DISCONNECT\n" @=? (N.printMessage N.Disconnect)

printDetachMessage :: Assertion
printDetachMessage = "DETACH\n" @=? (N.printMessage N.Detach)
