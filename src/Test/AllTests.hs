module Test.AllTests
where

import Test.HUnit

import qualified Vim.Netbeans as N

parseAuthMessage :: Assertion
parseAuthMessage = (Right $ N.EventMessage $ N.Auth "password")
                   @=?
                   (N.parseMessage "AUTH password")

parseVersionMessage :: Assertion
parseVersionMessage = (Right $ N.EventMessage $ N.Version 0 1 "2.5")
                      @=?
                      (N.parseMessage "0:version=1 \"2.5\"")

parseFileOpenedMessage :: Assertion
parseFileOpenedMessage = (Right $ N.EventMessage $ N.FileOpened
                                                    0 1 "pathname" True False)
                         @=?
                         (N.parseMessage "0:fileOpened=1 \"pathname\" T F")

parseKeyCommandMessage :: Assertion
parseKeyCommandMessage = (Right $ N.EventMessage $ N.KeyCommand 0 1 "F1")
                         @=?
                         (N.parseMessage "0:keyCommand=1 \"F1\"")

parseNewDotAndMarkMessage :: Assertion
parseNewDotAndMarkMessage = (Right $ N.EventMessage $ N.NewDotAndMark 0 1 3 4)
                            @=?
                            (N.parseMessage "0:newDotAndMark=1 3 4")

parseStartupDoneMessage :: Assertion
parseStartupDoneMessage = (Right $ N.EventMessage $ N.StartupDone 0 1)
                          @=?
                          (N.parseMessage "0:startupDone=1")

parseErrorMessage :: Assertion
parseErrorMessage = (Right $ N.EventMessage $ N.E463)
                    @=?
                    (N.parseMessage "E463")

printAddAnno :: Assertion
printAddAnno = "0:addAnno!1 2 3 10 5"
               @=?
               (N.printMessage $ N.CommandMessage $ N.AddAnno 0 1 2 3 10 5)

printClose :: Assertion
printClose = "0:close!1"
               @=?
               (N.printMessage $ N.CommandMessage $ N.Close 0 1)

printCreateMessage :: Assertion
printCreateMessage = "0:create!1"
                     @=?
                     (N.printMessage $ N.CommandMessage $ N.Create 0 1)

printDefineAnnoType :: Assertion
printDefineAnnoType = "0:defineAnnoType!1 2 \"typeName\" \"toolTip\" \"glyphFile\" Red Green"
                     @=?
                     (N.printMessage $ N.CommandMessage $ N.DefineAnnoType 0 1 2 "typeName" "toolTip" "glyphFile" N.Red N.Green)

printEditFileMessage :: Assertion
printEditFileMessage = "0:editFile!1 \"testfile.txt\""
                       @=?
                       (N.printMessage $ N.CommandMessage
                                                $ N.EditFile 0 1 "testfile.txt")

printEndAtomic :: Assertion
printEndAtomic = "0:endAtomic!1"
                 @=?
                 (N.printMessage $ N.CommandMessage $ N.EndAtomic 0 1)

printGuard :: Assertion
printGuard = "0:guard!1 10 14"
                 @=?
                 (N.printMessage $ N.CommandMessage $ N.Guard 0 1 10 14)

printDisconnectMessage :: Assertion
printDisconnectMessage = "DISCONNECT\n"
                         @=?
                         (N.printMessage $ N.CommandMessage N.DisconnectCommand)

printDetachMessage :: Assertion
printDetachMessage = "DETACH\n"
                     @=?
                     (N.printMessage $ N.CommandMessage $ N.Detach)

printSetReadOnlyMessage :: Assertion
printSetReadOnlyMessage = "0:setReadOnly!1"
                          @=?
                          (N.printMessage $ N.CommandMessage
                                                            $ N.SetReadOnly 0 1)
