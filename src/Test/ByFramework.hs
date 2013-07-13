import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.AllTests

import Test.HUnit

main = defaultMain tests

tests = [ testGroup "parseNumber"
            [ testCase "parseNumber" parseNumber
            , testCase "parseNumberNegative" parseNumberNegative
            ]
        , testGroup "parseMessage"
            [ testCase "parseAuthMessage" parseAuthMessage
            , testCase "parseBalloonText" parseBalloonText
            , testCase "parseButtonRelease" parseButtonRelease
            , testCase "parseDisconnect" parseDisconnect
            , testCase "parseFileOpenedMessage" parseFileOpenedMessage
            , testCase "parseGeometry" parseGeometry
            , testCase "parseInsert" parseInsert
            , testCase "parseKeyCommandMessage" parseKeyCommandMessage
            , testCase "parseKeyAtPos" parseKeyAtPos
            , testCase "parseKilled" parseKilled
            , testCase "parseNewDotAndMarkMessage" parseNewDotAndMarkMessage
            , testCase "parseRemove" parseRemove
            , testCase "parseSave" parseSave
            , testCase "parseStartupDoneMessage" parseStartupDoneMessage
            , testCase "parseUnmodified" parseUnmodified
            , testCase "parseVersionMessage" parseVersionMessage
            , testCase "parseErrorMessage" parseErrorMessage
            ]
        , testGroup "parseReply"
            [ testCase "parseGetCursorReply" parseGetCursorReply
            , testCase "parseGetLengthReply" parseGetLengthReply
            , testCase "parseGetAnnoReply" parseGetAnnoReply
            , testCase "parseGetModifiedReply" parseGetModifiedReply
            , testCase "parseGetTextReply" parseGetTextReply
            , testCase "parseInsertReplySuccess" parseInsertReplySuccess
            , testCase "parseInsertReplyError" parseInsertReplyError
            , testCase "parseRemoveReplySuccess" parseRemoveReplySuccess
            , testCase "parseRemoveReplyError" parseRemoveReplyError
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
