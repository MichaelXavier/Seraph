{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Seraph.Free where

import           Control.Exception
import           Prelude               hiding (log)
import           System.Posix.IO       (OpenFileFlags, OpenMode)
import           System.Posix.Process  (ProcessStatus)
import           System.Posix.Signals  (Signal)
import           System.Posix.Types    (Fd, GroupID, ProcessID, UserID)

import           Control.Monad.Free
import           Control.Monad.Free.TH

data SeraphView next = Log String next deriving (Functor)

makeFree ''SeraphView

type SeraphViewM = Free SeraphView

-- i think these "IO" ops here need to come back as SeraphChildM
data SeraphChild next = SetUserID UserID next
                      | SetGroupID GroupID next
                      | ChangeWorkingDirectory FilePath next
                      | ExecuteFile String [String] [(String, String)] (Either IOException ProcessID -> next)
                      | OpenFd FilePath OpenMode OpenFileFlags (Either IOException Fd -> next)
                      | DupTo Fd Fd next deriving (Functor)

makeFree ''SeraphChild

type SeraphChildM = Free SeraphChild

data SeraphProcess next = SignalProcess Signal ProcessID next
                        | WaitSecs Int next
                        | GetUserEntryForName String (Maybe UserID -> next)
                        | GetGroupEntryForName String (Maybe GroupID -> next)
                        | ForkProcess (SeraphChildM ()) (ProcessID -> next)
                        | GetProcessStatus ProcessID (Maybe ProcessStatus -> next)
                        deriving (Functor)

makeFree ''SeraphProcess

type SeraphProcessM = Free SeraphProcess
