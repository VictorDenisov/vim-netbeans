import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.AllTests

import Test.HUnit

main = defaultMain tests

tests = [ testGroup "parseMessage"
            [ testCase "parseAuthMessage" parseAuthMessage
            ]
        , testGroup "printMessage"
            [ testCase "printAuthMessage" printAuthMessage
            ]
        ]
