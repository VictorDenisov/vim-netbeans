module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (N.Auth "password") @=? (N.parseMessage "AUTH password")
