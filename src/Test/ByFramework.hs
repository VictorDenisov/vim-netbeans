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
            [ testCase "printDisconnectMessage" printDisconnectMessage
            , testCase "printDetachMessage" printDetachMessage
            , testCase "printCreateMessage" printCreateMessage
            , testCase "printDefineAnnoType" printDefineAnnoType
            , testCase "printAddAnno" printAddAnno
            , testCase "printEditFileMessage" printEditFileMessage
            , testCase "printEndAtomic" printEndAtomic
            , testCase "printGuard" printGuard
            , testCase "printSetReadOnlyMessage" printSetReadOnlyMessage
            , testCase "printClose" printClose
            ]
        ]
