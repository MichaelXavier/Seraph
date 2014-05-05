{-# LANGUAGE TemplateHaskell #-}
module Seraph.Types ( Config(..)
                    , HasConfig(..)
                    , ProgramId(..)
                    , HasProgramId(..)
                    , Program(..)
                    , HasProgram(..)
                    ) where

import Control.Lens (makeClassy)
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
