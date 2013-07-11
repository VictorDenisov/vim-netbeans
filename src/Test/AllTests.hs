module Test.AllTests
where

import Test.HUnit
import Text.ParserCombinators.Parsec (parse)

import qualified Vim.Netbeans.Protocol as N

parseNumber :: Assertion
parseNumber = case parse N.parseNumber "(unknown)" "123" of
                Right n -> n @=? 123
                Left _ -> assertFailure "parsing failure"

parseNumberNegative :: Assertion
parseNumberNegative = case parse N.parseNumber "(unknown)" "-12" of
                Right n -> n @=? (-12)
                Left _ -> assertFailure "parsing failure"

parseAuthMessage :: Assertion
parseAuthMessage = (Right $ N.EventMessage (-1) (-1) $ N.Auth "password")
                   @=?
                   (N.parseMessage [] "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (Right $ N.EventMessage 0 1 $ N.Version "2.5")
                      @=?
                      (N.parseMessage [] "0:version=1 \"2.5\"")

parseFileOpenedMessage :: Assertion
parseFileOpenedMessage = (Right $ N.EventMessage 0 1 $ N.FileOpened
                                                    "pathname" True False)
                         @=?
                         (N.parseMessage [] "0:fileOpened=1 \"pathname\" T F")

parseKeyCommandMessage :: Assertion
parseKeyCommandMessage = (Right $ N.EventMessage 0 1 $ N.KeyCommand "F1")
                         @=?
                         (N.parseMessage [] "0:keyCommand=1 \"F1\"")

parseNewDotAndMarkMessage :: Assertion
parseNewDotAndMarkMessage = (Right $ N.EventMessage 0 1 $ N.NewDotAndMark 3 4)
                            @=?
                            (N.parseMessage [] "0:newDotAndMark=1 3 4")

parseStartupDoneMessage :: Assertion
parseStartupDoneMessage = (Right $ N.EventMessage 0 1 $ N.StartupDone)
                          @=?
                          (N.parseMessage [] "0:startupDone=1")

parseErrorMessage :: Assertion
parseErrorMessage = (Right $ N.EventMessage (-1) (-1) $ N.E463)
                    @=?
                    (N.parseMessage [] "E463")

parseGetCursorReply :: Assertion
parseGetCursorReply = (Right $ N.ReplyMessage 0 $ N.GetCursorReply 1 2 3 4)
                      @=?
                      (N.parseMessage [(0, N.getCursorReplyParser)] "0 1 2 3 4")

parseGetLengthReply :: Assertion
parseGetLengthReply = (Right $ N.ReplyMessage 1 $ N.GetLengthReply 2)
                      @=?
                      (N.parseMessage [(1, N.getLengthReplyParser)] "1 2")

parseGetAnnoReply :: Assertion
parseGetAnnoReply = (Right $ N.ReplyMessage 1 $ N.GetAnnoReply 2)
                      @=?
                      (N.parseMessage [(1, N.getAnnoReplyParser)] "1 2")

parseGetModifiedReply :: Assertion
parseGetModifiedReply = (Right $ N.ReplyMessage 1 $ N.GetModifiedReply 1)
                        @=?
                        (N.parseMessage [(1, N.getModifiedReplyParser)] "1 1")

printDisconnectMessage :: Assertion
printDisconnectMessage = "DISCONNECT\n"
                         @=?
                         (N.printMessage $ N.DisconnectCommand)

printDetachMessage :: Assertion
printDetachMessage = "DETACH\n"
                     @=?
                     (N.printMessage $ N.Detach)

printAddAnno :: Assertion
printAddAnno = "0:addAnno!1 2 3 10 5"
               @=?
               (N.printMessage $ N.CommandMessage 0 1 $ N.AddAnno 2 3 10 5)

printClose :: Assertion
printClose = "0:close!1"
               @=?
               (N.printMessage $ N.CommandMessage 0 1 N.Close)

printCreateMessage :: Assertion
printCreateMessage = "0:create!1"
                     @=?
                     (N.printMessage $ N.CommandMessage 0 1 N.Create)

printDefineAnnoType :: Assertion
printDefineAnnoType = "0:defineAnnoType!1 2 \"typeName\" \"toolTip\" \"glyphFile\" Red Green"
                     @=?
                     (N.printMessage $ N.CommandMessage 0 1 $ N.DefineAnnoType 2 "typeName" "toolTip" "glyphFile" N.Red N.Green)

printEditFileMessage :: Assertion
printEditFileMessage = "0:editFile!1 \"testfile.txt\""
                       @=?
                       (N.printMessage $ N.CommandMessage 0 1
                                                $ N.EditFile "testfile.txt")

printEndAtomic :: Assertion
printEndAtomic = "0:endAtomic!1"
                 @=?
                 (N.printMessage $ N.CommandMessage 0 1 N.EndAtomic)

printGuard :: Assertion
printGuard = "0:guard!1 10 14"
             @=?
             (N.printMessage $ N.CommandMessage 0 1 $ N.Guard 10 14)

printInitDone :: Assertion
printInitDone = "0:initDone!1"
                @=?
                (N.printMessage $ N.CommandMessage 0 1 N.InitDone)

printInsertDone :: Assertion
printInsertDone = "0:insertDone!1"
                  @=?
                  (N.printMessage $ N.CommandMessage 0 1 N.InsertDone)

printNetbeansBuffer :: Assertion
printNetbeansBuffer = "0:netbeansBuffer!1 T"
                      @=?
                      (N.printMessage $ N.CommandMessage 0 1
                                                    $ N.NetbeansBuffer True)

printPutBufferNumber :: Assertion
printPutBufferNumber = "0:putBufferNumber!1 \"path\""
                       @=?
                       (N.printMessage $ N.CommandMessage 0 1
                                                 $ N.PutBufferNumber "path")

printRaise :: Assertion
printRaise = "0:raise!1"
             @=?
             (N.printMessage $ N.CommandMessage 0 1 N.Raise)

printRemoveAnno :: Assertion
printRemoveAnno = "0:removeAnno!1 3"
                  @=?
                  (N.printMessage $ N.CommandMessage 0 1 $ N.RemoveAnno 3)

printSave :: Assertion
printSave = "0:save!1"
            @=?
            (N.printMessage $ N.CommandMessage 0 1 N.Save)

printSaveDone :: Assertion
printSaveDone = "0:saveDone!1"
                @=?
                (N.printMessage $ N.CommandMessage 0 1 N.SaveDone)

printSetBufferNumber :: Assertion
printSetBufferNumber = "0:setBufferNumber!1 \"path\""
                     @=?
                     (N.printMessage $ N.CommandMessage 0 1
                                                    $ N.SetBufferNumber "path")

printSetDot :: Assertion
printSetDot = "0:setDot!1 10"
              @=?
              (N.printMessage $ N.CommandMessage 0 1 $ N.SetDot 10)

printSetExitDelay :: Assertion
printSetExitDelay = "0:setExitDelay!1 10"
                    @=?
                    (N.printMessage $ N.CommandMessage 0 1 $ N.SetExitDelay 10)

printSetFullName :: Assertion
printSetFullName = "0:setFullName!1 \"fullName\""
                    @=?
                    (N.printMessage $ N.CommandMessage 0 1
                                                    $ N.SetFullName "fullName")

printSetModified :: Assertion
printSetModified = "0:setModified!1 T"
                    @=?
                    (N.printMessage $ N.CommandMessage 0 1
                                                    $ N.SetModified True)

printSetReadOnlyMessage :: Assertion
printSetReadOnlyMessage = "0:setReadOnly!1"
                          @=?
                          (N.printMessage $ N.CommandMessage 0 1 N.SetReadOnly)

printSetTitle :: Assertion
printSetTitle = "0:setTitle!1 \"title\""
                @=?
                (N.printMessage $ N.CommandMessage 0 1 $ N.SetTitle "title")

printSetVisible :: Assertion
printSetVisible = "0:setVisible!1 T"
                @=?
                (N.printMessage $ N.CommandMessage 0 1 $ N.SetVisible True)

printShowBalloon :: Assertion
printShowBalloon = "0:showBalloon!1 \"text\""
                @=?
                (N.printMessage $ N.CommandMessage 0 1 $ N.ShowBalloon "text")

printSpecialKeys :: Assertion
printSpecialKeys = "0:specialKeys!1"
                   @=?
                   (N.printMessage $ N.CommandMessage 0 1 N.SpecialKeys)

printStartAtomic :: Assertion
printStartAtomic = "0:startAtomic!1"
                   @=?
                   (N.printMessage $ N.CommandMessage 0 1 N.StartAtomic)

printStartDocumentListen :: Assertion
printStartDocumentListen = "0:startDocumentListen!1"
                           @=?
                           (N.printMessage $ N.CommandMessage 0 1 N.StartDocumentListen)

printStopDocumentListen :: Assertion
printStopDocumentListen = "0:stopDocumentListen!1"
                          @=?
                          (N.printMessage $ N.CommandMessage 0 1 N.StopDocumentListen)

printUnguard :: Assertion
printUnguard = "0:unguard!1 10 14"
               @=?
               (N.printMessage $ N.CommandMessage 0 1 $ N.Unguard 10 14)

printGetCursor :: Assertion
printGetCursor = "0:getCursor/1"
                 @=?
                 (N.printMessage $ N.FunctionMessage 0 1 $ N.GetCursor)

printGetLength :: Assertion
printGetLength = "0:getLength/1"
                 @=?
                 (N.printMessage $ N.FunctionMessage 0 1 $ N.GetLength)

printGetAnno :: Assertion
printGetAnno = "0:getAnno/1 10"
                 @=?
                 (N.printMessage $ N.FunctionMessage 0 1 $ N.GetAnno 10)

printGetModified :: Assertion
printGetModified = "0:getModified/1"
                   @=?
                   (N.printMessage $ N.FunctionMessage 0 1 $ N.GetModified)

printGetText :: Assertion
printGetText = "0:getText/1"
               @=?
               (N.printMessage $ N.FunctionMessage 0 1 $ N.GetText)

printInsert :: Assertion
printInsert = "0:insert/1 10 \"hello\""
               @=?
               (N.printMessage $ N.FunctionMessage 0 1 $ N.Insert 10 "hello")

printRemove :: Assertion
printRemove = "0:remove/1 10 12"
              @=?
              (N.printMessage $ N.FunctionMessage 0 1 $ N.Remove 10 12)

printSaveAndExit :: Assertion
printSaveAndExit = "0:saveAndExit/1"
              @=?
              (N.printMessage $ N.FunctionMessage 0 1 $ N.SaveAndExit)
