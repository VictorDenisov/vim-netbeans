import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.AllTests

import Test.HUnit

main = defaultMain tests

tests = [ testGroup "parseNumber"
            [ testCase "parseNumber" parseNumber
            , testCase "parseNumberNegative" parseNumberNegative
            ]
        , testGroup "parseEvent"
            [ testCase "parseAuth" parseAuth
            , testCase "parseBalloonText" parseBalloonText
            , testCase "parseButtonRelease" parseButtonRelease
            , testCase "parseDisconnect" parseDisconnect
            , testCase "parseFileOpened" parseFileOpened
            , testCase "parseGeometry" parseGeometry
            , testCase "parseInsert" parseInsert
            , testCase "parseKeyCommand" parseKeyCommand
            , testCase "parseKeyAtPos" parseKeyAtPos
            , testCase "parseKilled" parseKilled
            , testCase "parseNewDotAndMark" parseNewDotAndMark
            , testCase "parseRemove" parseRemove
            , testCase "parseSave" parseSave
            , testCase "parseStartupDone" parseStartupDone
            , testCase "parseUnmodified" parseUnmodified
            , testCase "parseVersion" parseVersion
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
            [ testCase "printDisconnect" printDisconnect
            , testCase "printDetach" printDetach
            , testCase "printAddAnno" printAddAnno
            , testCase "printClose" printClose
            , testCase "printCreate" printCreate
            , testCase "printDefineAnnoType" printDefineAnnoType
            , testCase "printEditFile" printEditFile
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
            , testCase "printSetReadOnly" printSetReadOnly
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
