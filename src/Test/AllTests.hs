module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (N.Auth "password") @=? (N.parseMessage "AUTH password")

printAuthMessage :: Assertion
printAuthMessage = ("AUTH password\n") @=? (N.printMessage (N.Auth "password"))
