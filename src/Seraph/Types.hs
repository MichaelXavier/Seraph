{-# LANGUAGE TemplateHaskell #-}
module Seraph.Types ( Config(..)
                    , HasConfig(..)
                    , ProgramId(..)
                    , HasProgramId(..)
                    , Program(..)
                    , HasProgram(..)
                    , Directive(..)
                    , Directives(..)
                    , Event(..)
                    , LogCtx(..)
                    , HasLogCtx(..)
                    , SpawnError(..)
                    , KillPolicy(..)
                    , ProcessHandle(..)
                    , HasProcessHandle(..)
                    , killPolicy
                    ) where

import Control.Lens
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import System.Posix.Types (ProcessID)

newtype ProgramId = ProgramId { _pidStr :: String} deriving (Show, Eq, Ord)

makeClassy ''ProgramId

--TODO: use real filepaths
--TODO: uid, guid using getAllGroupEntries
data Program = Program {
  _name       :: ProgramId,
  _exec       :: String,
  _delay      :: Int,
  _userName   :: Maybe String,
  _groupName  :: Maybe String,
  _stdout     :: Maybe FilePath,
  _stderr     :: Maybe FilePath,
  _workingDir :: Maybe FilePath,
  _logExec    :: Maybe String,
  _pidFile    :: Maybe FilePath,
  _env        :: [(String, String)],
  _termGrace  :: Maybe Int -- ^ How long to wait in seconds after sending a SIGTERM before SIGKILL. Nothing = never SIGKILL. Default Nothing
} deriving (Show, Eq, Ord)

makeClassy ''Program

data Config = Config { _configured :: Map ProgramId Program
                     , _running    :: Set ProgramId
                     } deriving (Show, Eq)
makeClassy ''Config

instance Monoid Config where
  mempty = Config mempty mempty
  c1 `mappend` c2 = c1 & configured <>~ c2 ^. configured
                       & running    <>~ c2 ^. running

data Directive = SpawnProg Program
               | KillProg ProgramId deriving (Show, Eq)

makePrisms ''Directive

data Directives = Directives [Directive]
                | FinalDirectives [Directive] deriving (Show, Eq)

--TODO: proof
instance Monoid Directives where
  mempty = Directives mempty
  (Directives as) `mappend` (Directives bs) = Directives $ as `mappend` bs
  (Directives as) `mappend` (FinalDirectives bs) = FinalDirectives $ as `mappend` bs
  (FinalDirectives as) `mappend` (FinalDirectives bs) = FinalDirectives $ as `mappend` bs
  (FinalDirectives as) `mappend` (Directives bs) = FinalDirectives $ as `mappend` bs

makePrisms ''Directives

data SpawnError = InvalidExec
                | InvalidUser
                | InvalidGroup
                | SpawnException IOError deriving (Show, Eq)

data Event = NewConfig Config
           | ProcessDeath ProgramId
           | ProgRunning ProgramId
           | ProgNotStarted ProgramId SpawnError
           | ShutdownRequested deriving (Show, Eq)

makePrisms ''Event

newtype LogCtx = LogCtx { _ctx :: String }

makeClassy ''LogCtx

data KillPolicy = SoftKill | HardKill Int deriving (Show, Eq)

data ProcessHandle = ProcessHandle { _pid    :: ProcessID
                                   , _policy :: KillPolicy } deriving (Show, Eq)

makeClassy ''ProcessHandle

killPolicy :: Program -> KillPolicy
killPolicy Program { _termGrace = Just n } = HardKill n
killPolicy _                               = SoftKill
