{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Seraph.Free where

import Prelude hiding (log)
import Control.Lens
import System.Posix.IO ( OpenMode
                       , OpenFileFlags )
import System.Posix.Process ( ProcessStatus )
import System.Posix.Types ( ProcessID
                          , GroupID
                          , UserID
                          , Fd )
import System.Posix.Signals ( Signal
                            , sigTERM
                            , sigKILL )

import Control.Monad.Free
import Control.Monad.Free.TH

import Seraph.Types

data SeraphView next = Log String next deriving (Functor)

makeFree ''SeraphView

type SeraphViewM = Free SeraphView

data SeraphChild next = SetUserID UserID next
                      | SetGroupID GroupID next
                      | ChangeWorkingDirectory FilePath next
                      | ExecuteFile String [String] [(String, String)] (ProcessID -> next)
                      | OpenFd FilePath OpenMode OpenFileFlags (Fd -> next)
                      | DupTo Fd Fd next deriving (Functor)

makeFree ''SeraphChild

type SeraphChildM = Free SeraphChild

data SeraphProcess next = SignalProcess Signal ProcessID next
                        | WaitSecs Int next
                        | GetUserEntryForName String (Maybe UserID -> next)
                        | GetGroupEntryForName String (Maybe GroupID -> next)
                        | ForkProcess (SeraphChild ProcessID -> next)
                        | GetProcessStatus Bool Bool ProcessID (Maybe ProcessStatus -> next)
                        deriving (Functor)

makeFree ''SeraphProcess

type SeraphProcessM = Free SeraphProcess
