module Test.AllTests
where

import Test.HUnit
import Text.ParserCombinators.Parsec (parse)

import qualified Vim.Netbeans.Protocol as N

parseNumber :: Assertion
parseNumber = case parse N.parseNumber "(unknown)" "123" of
                Right n -> n @=? 123
                Left _ -> assertFailure "parsing failure"

parseString :: Assertion
parseString = case parse N.parseString "(unknown)" "\"text\"" of
                Right n -> n @=? "text"
                Left _ -> assertFailure "parsing failure"

parseNumberNegative :: Assertion
parseNumberNegative = case parse N.parseNumber "(unknown)" "-12" of
                Right n -> n @=? (-12)
                Left _ -> assertFailure "parsing failure"

parseBalloonText :: Assertion
parseBalloonText = (Right $ N.EventMessage (N.BufId 0) 1 $ N.BalloonText "text")
                   @=?
                   (N.parseMessage [] "0:balloonText=1 \"text\"")

parseButtonRelease :: Assertion
parseButtonRelease = (Right $ N.EventMessage (N.BufId 0) 1 $ N.ButtonRelease 2 3 4)
                     @=?
                     (N.parseMessage [] "0:buttonRelease=1 2 3 4")

parseDisconnect :: Assertion
parseDisconnect = (Right $ N.EventMessage (N.BufId 0) 1 $ N.Disconnect)
                  @=?
                  (N.parseMessage [] "0:disconnect=1")

parseGeometry :: Assertion
parseGeometry = (Right $ N.EventMessage (N.BufId 0) 1 $ N.Geometry 2 3 4 5)
                @=?
                (N.parseMessage [] "0:geometry=1 2 3 4 5")

parseInsert :: Assertion
parseInsert = (Right $ N.EventMessage (N.BufId 0) 1 $ N.InsertEvent 2 "text")
              @=?
              (N.parseMessage [] "0:insert=1 2 \"text\"")

parseKeyCommand :: Assertion
parseKeyCommand = (Right $ N.EventMessage (N.BufId 0) 1 $ N.KeyCommand "F1")
                         @=?
                         (N.parseMessage [] "0:keyCommand=1 \"F1\"")

parseKeyAtPos :: Assertion
parseKeyAtPos = (Right $ N.EventMessage (N.BufId 0) 1 $ N.KeyAtPos "F1" 2 3)
                @=?
                (N.parseMessage [] "0:keyAtPos=1 \"F1\" 2/3")

parseKilled :: Assertion
parseKilled = (Right $ N.EventMessage (N.BufId 0) 1 $ N.Killed)
              @=?
              (N.parseMessage [] "0:killed=1")

parseNewDotAndMark :: Assertion
parseNewDotAndMark = (Right $ N.EventMessage (N.BufId 0) 1 $ N.NewDotAndMark 3 4)
                            @=?
                            (N.parseMessage [] "0:newDotAndMark=1 3 4")

parseRemove :: Assertion
parseRemove = (Right $ N.EventMessage (N.BufId 0) 1 $ N.RemoveEvent 2 3)
              @=?
              (N.parseMessage [] "0:remove=1 2 3")

parseSave :: Assertion
parseSave = (Right $ N.EventMessage (N.BufId 0) 1 $ N.SaveEvent)
            @=?
            (N.parseMessage [] "0:save=1")

parseStartupDone :: Assertion
parseStartupDone = (Right $ N.EventMessage (N.BufId 0) 1 $ N.StartupDone)
                          @=?
                          (N.parseMessage [] "0:startupDone=1")

parseUnmodified :: Assertion
parseUnmodified = (Right $ N.EventMessage (N.BufId 0) 1 $ N.Unmodified)
                  @=?
                  (N.parseMessage [] "0:unmodified=1")

parseAuth :: Assertion
parseAuth = (Right $ N.EventMessage (N.BufId (-1)) (-1) $ N.Auth "password")
                   @=?
                   (N.parseMessage [] "AUTH password")

parseVersion :: Assertion
parseVersion = (Right $ N.EventMessage (N.BufId 0) 1 $ N.Version "2.5")
                      @=?
                      (N.parseMessage [] "0:version=1 \"2.5\"")

parseFileOpened :: Assertion
parseFileOpened = (Right $ N.EventMessage (N.BufId 0) 1 $ N.FileOpened
                                                    "pathname" True False)
                         @=?
                         (N.parseMessage [] "0:fileOpened=1 \"pathname\" T F")

parseGetCursorReply :: Assertion
parseGetCursorReply = (Right $ N.ReplyMessage 0 $ N.GetCursorReply (N.BufId 1) 2 3 4)
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

parseGetTextReply :: Assertion
parseGetTextReply = (Right $ N.ReplyMessage 1 $ N.GetTextReply "text")
                        @=?
                        (N.parseMessage [(1, N.getTextReplyParser)] "1 \"text\"")

parseInsertReplySuccess :: Assertion
parseInsertReplySuccess = (Right $ N.ReplyMessage 1 $ N.InsertReplySuccess)
                              @=?
                              (N.parseMessage [(1, N.insertReplyParser)] "1")

parseInsertReplyError :: Assertion
parseInsertReplyError = (Right $ N.ReplyMessage 1 $ N.InsertReplyError "error message")
                        @=?
                        (N.parseMessage [(1, N.insertReplyParser)] "1 !error message")

parseRemoveReplySuccess :: Assertion
parseRemoveReplySuccess = (Right $ N.ReplyMessage 1 $ N.RemoveReplySuccess)
                              @=?
                              (N.parseMessage [(1, N.removeReplyParser)] "1")

parseRemoveReplyError :: Assertion
parseRemoveReplyError = (Right $ N.ReplyMessage 1 $ N.RemoveReplyError "error message")
                        @=?
                        (N.parseMessage [(1, N.removeReplyParser)] "1 !error message")

printDisconnect :: Assertion
printDisconnect = "DISCONNECT\n"
                         @=?
                         (N.printMessage $ N.DisconnectCommand)

printDetach :: Assertion
printDetach = "DETACH\n"
                     @=?
                     (N.printMessage $ N.Detach)

printAddAnno :: Assertion
printAddAnno = "0:addAnno!1 2 3 10 5"
               @=?
               (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $
                                            N.AddAnno 2 (N.AnnoTypeNum 3) 10 5)

printClose :: Assertion
printClose = "0:close!1"
               @=?
               (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.Close)

printCreate :: Assertion
printCreate = "0:create!1"
                     @=?
                     (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.Create)

printDefineAnnoType :: Assertion
printDefineAnnoType = "0:defineAnnoType!1 2 \"typeName\" \"toolTip\" \"glyphFile\" Red Green"
                     @=?
                     (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $
                                              N.DefineAnnoType
                                                    (N.AnnoTypeNum 2)
                                                    "typeName"
                                                    "toolTip"
                                                    "glyphFile"
                                                    N.Red
                                                    N.Green)

printEditFile :: Assertion
printEditFile = "0:editFile!1 \"testfile.txt\""
                       @=?
                       (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                $ N.EditFile "testfile.txt")

printEndAtomic :: Assertion
printEndAtomic = "0:endAtomic!1"
                 @=?
                 (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.EndAtomic)

printGuard :: Assertion
printGuard = "0:guard!1 10 14"
             @=?
             (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.Guard 10 14)

printInitDone :: Assertion
printInitDone = "0:initDone!1"
                @=?
                (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.InitDone)

printInsertDone :: Assertion
printInsertDone = "0:insertDone!1"
                  @=?
                  (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.InsertDone)

printNetbeansBuffer :: Assertion
printNetbeansBuffer = "0:netbeansBuffer!1 T"
                      @=?
                      (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                    $ N.NetbeansBuffer True)

printPutBufferNumber :: Assertion
printPutBufferNumber = "0:putBufferNumber!1 \"path\""
                       @=?
                       (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                 $ N.PutBufferNumber "path")

printRaise :: Assertion
printRaise = "0:raise!1"
             @=?
             (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.Raise)

printRemoveAnno :: Assertion
printRemoveAnno = "0:removeAnno!1 3"
                  @=?
                  (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.RemoveAnno 3)

printSave :: Assertion
printSave = "0:save!1"
            @=?
            (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.Save)

printSaveDone :: Assertion
printSaveDone = "0:saveDone!1"
                @=?
                (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.SaveDone)

printSetBufferNumber :: Assertion
printSetBufferNumber = "0:setBufferNumber!1 \"path\""
                     @=?
                     (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                    $ N.SetBufferNumber "path")

printSetDot :: Assertion
printSetDot = "0:setDot!1 10"
              @=?
              (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.SetDot 10)

printSetExitDelay :: Assertion
printSetExitDelay = "0:setExitDelay!1 10"
                    @=?
                    (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.SetExitDelay 10)

printSetFullName :: Assertion
printSetFullName = "0:setFullName!1 \"fullName\""
                    @=?
                    (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                    $ N.SetFullName "fullName")

printSetModified :: Assertion
printSetModified = "0:setModified!1 T"
                    @=?
                    (N.printMessage $ N.CommandMessage (N.BufId 0) 1
                                                    $ N.SetModified True)

printSetReadOnly :: Assertion
printSetReadOnly = "0:setReadOnly!1"
                          @=?
                          (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.SetReadOnly)

printSetTitle :: Assertion
printSetTitle = "0:setTitle!1 \"title\""
                @=?
                (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.SetTitle "title")

printSetVisible :: Assertion
printSetVisible = "0:setVisible!1 T"
                @=?
                (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.SetVisible True)

printShowBalloon :: Assertion
printShowBalloon = "0:showBalloon!1 \"text\""
                @=?
                (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.ShowBalloon "text")

printSpecialKeys :: Assertion
printSpecialKeys = "0:specialKeys!1"
                   @=?
                   (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.SpecialKeys)

printStartAtomic :: Assertion
printStartAtomic = "0:startAtomic!1"
                   @=?
                   (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.StartAtomic)

printStartDocumentListen :: Assertion
printStartDocumentListen = "0:startDocumentListen!1"
                           @=?
                           (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.StartDocumentListen)

printStopDocumentListen :: Assertion
printStopDocumentListen = "0:stopDocumentListen!1"
                          @=?
                          (N.printMessage $ N.CommandMessage (N.BufId 0) 1 N.StopDocumentListen)

printUnguard :: Assertion
printUnguard = "0:unguard!1 10 14"
               @=?
               (N.printMessage $ N.CommandMessage (N.BufId 0) 1 $ N.Unguard 10 14)

printGetCursor :: Assertion
printGetCursor = "0:getCursor/1"
                 @=?
                 (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.GetCursor)

printGetLength :: Assertion
printGetLength = "0:getLength/1"
                 @=?
                 (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.GetLength)

printGetAnno :: Assertion
printGetAnno = "0:getAnno/1 10"
                 @=?
                 (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.GetAnno 10)

printGetModified :: Assertion
printGetModified = "0:getModified/1"
                   @=?
                   (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.GetModified)

printGetText :: Assertion
printGetText = "0:getText/1"
               @=?
               (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.GetText)

printInsert :: Assertion
printInsert = "0:insert/1 10 \"hello\""
               @=?
               (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.Insert 10 "hello")

printRemove :: Assertion
printRemove = "0:remove/1 10 12"
              @=?
              (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.Remove 10 12)

printSaveAndExit :: Assertion
printSaveAndExit = "0:saveAndExit/1"
              @=?
              (N.printMessage $ N.FunctionMessage (N.BufId 0) 1 $ N.SaveAndExit)
