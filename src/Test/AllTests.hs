module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (Right $ N.Auth "password")
                   @=?
                   (N.parseMessage "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (Right $ N.EventMessage 0 1 $ N.Version "2.5")
                      @=?
                      (N.parseMessage "0:version=1 \"2.5\"")

parseFileOpenedMessage :: Assertion
parseFileOpenedMessage = (Right $ N.EventMessage 0 1 $ N.FileOpened
                                                    "pathname" True False)
                         @=?
                         (N.parseMessage "0:fileOpened=1 \"pathname\" T F")

parseKeyCommandMessage :: Assertion
parseKeyCommandMessage = (Right $ N.EventMessage 0 1 $ N.KeyCommand "F1")
                         @=?
                         (N.parseMessage "0:keyCommand=1 \"F1\"")

parseNewDotAndMarkMessage :: Assertion
parseNewDotAndMarkMessage = (Right $ N.EventMessage 0 1 $ N.NewDotAndMark 3 4)
                            @=?
                            (N.parseMessage "0:newDotAndMark=1 3 4")

parseStartupDoneMessage :: Assertion
parseStartupDoneMessage = (Right $ N.EventMessage 0 1 $ N.StartupDone)
                          @=?
                          (N.parseMessage "0:startupDone=1")

parseErrorMessage :: Assertion
parseErrorMessage = (Right $ N.E463)
                    @=?
                    (N.parseMessage "E463")

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

printSetReadOnlyMessage :: Assertion
printSetReadOnlyMessage = "0:setReadOnly!1"
                          @=?
                          (N.printMessage $ N.CommandMessage 0 1 N.SetReadOnly)
