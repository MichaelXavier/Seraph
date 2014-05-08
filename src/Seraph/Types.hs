{-# LANGUAGE TemplateHaskell #-}
module Seraph.Types ( Config(..)
                    , HasConfig(..)
                    , ProgramId(..)
                    , HasProgramId(..)
                    , Program(..)
                    , HasProgram(..)
                    , Directive(..)
                    , Event(..)
                    ) where

import Control.Lens
import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)

newtype ProgramId = ProgramId { _pidStr :: String} deriving (Show, Eq, Ord)

makeClassy ''ProgramId

data Program = Program {
  _name       :: ProgramId,
  _exec       :: Maybe String,
  _delay      :: Maybe Int,
  _stdout     :: Maybe String,
  _stderr     :: Maybe String,
  _workingDir :: Maybe FilePath,
  _logExec    :: Maybe String,
  _pidFile    :: Maybe FilePath,
  _env        :: [(String, String)],
  _termGrace  :: Maybe Int -- ^ How long to wait after sending a SIGTERM before SIGKILL. Nothing = never SIGKILL. Default Nothing
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

data Directive = SpawnProgs [Program]
               | KillProgs [ProgramId]
               | Exit deriving (Show, Eq)

data Event = NewConfig Config
           | ProcessDeath ProgramId
           | ProgRunning ProgramId
           | ShutdownRequested deriving (Show, Eq)
