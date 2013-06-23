import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.AllTests

import Test.HUnit

main = defaultMain tests

tests = [ testGroup "parseMessage"
            [ testCase "parseAuthMessage" parseAuthMessage
            , testCase "parseFileOpenedMessage" parseFileOpenedMessage
            , testCase "parseVersionMessage" parseVersionMessage
            , testCase "parseKeyCommandMessage" parseKeyCommandMessage
            , testCase "parseNewDotAndMarkMessage" parseNewDotAndMarkMessage
            , testCase "parseStartupDoneMessage" parseStartupDoneMessage
            , testCase "parseErrorMessage" parseErrorMessage
            ]
        , testGroup "printMessage"
            [ testCase "printAuthMessage" printAuthMessage
            , testCase "printDisconnectMessage" printDisconnectMessage
            , testCase "printDetachMessage" printDetachMessage
            , testCase "printCreateMessage" printCreateMessage
            , testCase "printEditFileMessage" printEditFileMessage
            , testCase "printSetReadOnlyMessage" printSetReadOnlyMessage
            ]
        ]
