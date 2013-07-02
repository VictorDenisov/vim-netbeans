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
        , testGroup "printCommand"
            [ testCase "printDisconnectMessage" printDisconnectMessage
            , testCase "printDetachMessage" printDetachMessage
            , testCase "printAddAnno" printAddAnno
            , testCase "printClose" printClose
            , testCase "printCreateMessage" printCreateMessage
            , testCase "printDefineAnnoType" printDefineAnnoType
            , testCase "printEditFileMessage" printEditFileMessage
            , testCase "printEndAtomic" printEndAtomic
            , testCase "printGuard" printGuard
            , testCase "printInitDone" printInitDone
            , testCase "printInsertDone" printInsertDone
            , testCase "printNetbeansBuffer" printNetbeansBuffer
            , testCase "printPutBufferNumber" printPutBufferNumber
            , testCase "printRaise" printRaise
            , testCase "printRemoveAnno" printRemoveAnno
            , testCase "printSave" printSave
            , testCase "printSaveDone" printSaveDone
            , testCase "printSetBufferNumber" printSetBufferNumber
            , testCase "printSetDot" printSetDot
            , testCase "printSetExitDelay" printSetExitDelay
            , testCase "printSetFullName" printSetFullName
            , testCase "printSetModified" printSetModified
            , testCase "printSetReadOnlyMessage" printSetReadOnlyMessage
            , testCase "printSetTitle" printSetTitle
            , testCase "printSetVisible" printSetVisible
            , testCase "printShowBalloon" printShowBalloon
            , testCase "printSpecialKeys" printSpecialKeys
            , testCase "printStartAtomic" printStartAtomic
            , testCase "printStartDocumentListen" printStartDocumentListen
            , testCase "printStopDocumentListen" printStopDocumentListen
            , testCase "printUnguard" printUnguard
            ]
        , testGroup "printFunction"
            [ testCase "printGetCursor" printGetCursor
            , testCase "printGetLength" printGetLength
            , testCase "printGetAnno" printGetAnno
            , testCase "printGetModified" printGetModified
            , testCase "printGetText" printGetText
            , testCase "printInsert" printInsert
            , testCase "printRemove" printRemove
            , testCase "printSaveAndExit" printSaveAndExit
            ]
        ]
