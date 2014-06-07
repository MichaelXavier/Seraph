{-# LANGUAGE FlexibleContexts #-}
module Seraph.Model ( Directive(..)
                    , Event(..)
                    , oracle
                    , oracleModel
                    , oracleDebug
                    ) where

import Data.Maybe ( isJust
                  , mapMaybe
                  )
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Set ( (\\)
                , Set
                )
import qualified Data.Set as S
import qualified Data.Map as M
import MVC

import Debug.Trace

import Seraph.Types

oracleModel :: Model Config Event ([Directive], [String])
oracleModel = asPipe $ loop model
  where
    model = unwrapModel oracle
    -- model = unwrapModel oracleDebug

unwrapModel :: (Event -> WriterT [String] (State s) a)
          -> Event
          -> ListT (State s) (a, [String])
unwrapModel f e = lift $ runWriterT (f e)

oracleDebug :: Event -> WriterT [String] (State Config) [Directive]
oracleDebug e = do
  s <- get
  res <- traceShow s $ oracle e
  -- s' <- get
  -- traceShow ("EVT", e, "BEFORE", s, "AFTER", s') $ return res
  traceShow ("EVT", e, "RES", res) $ return res

oracle :: Event -> WriterT [String] (State Config) [Directive]
oracle (ProcessDeath prid) = do
  existing <- gets $ view $ running . at prid . to isJust
  mainLogger $ if existing
     then "Spawning " ++ prid ^. pidStr
     else "Unknown process " ++ prid ^. pidStr
  modify $ set (running . at prid) Nothing
  progs <- gets $ toListOf $ configured . ix prid
  return $ if null progs
           then []
           else [SpawnProgs progs]
oracle (ProgRunning prid) = do
  modify $ \c -> c & running <>~ S.singleton prid
  mainLogger $ "Marked " ++ show prid ++ " as running"
  return []
oracle (NewConfig cfg) = do
  oldCfg <- get
  let oldPids = configPids oldCfg
  let newPids = configPids cfg
  let currentlyRunning = oldCfg ^. running
  let spawnPids = newPids \\ oldPids
  let killPids = currentlyRunning \\ newPids
  let spawnProgs = mapMaybe (\prid -> cfg ^. configured . at prid) (spawnPids ^. to S.toList)
  put cfg
  return [SpawnProgs spawnProgs, KillProgs (killPids ^. to S.toList)]
oracle ShutdownRequested = do
  modify $ set configured mempty
  killPids <- gets $ view (running . to S.toList)
  return [KillProgs killPids , Exit]
oracle (ProgNotStarted prid e) = do
  --TODO: nicer formatting
  --TODO: stop flapping or keep going?
  ctxLogger (prid ^. pidStr) $ "Failed to start: " ++ show e
  return []

configPids :: Config -> Set ProgramId
configPids cfg = cfg ^. configured . to M.keys . to S.fromList

mainLogger :: MonadWriter [String] m => String -> m ()
mainLogger = ctxLogger "seraph"

ctxLogger :: MonadWriter [String] m =>  String -> String -> m ()
ctxLogger label s = tell [unwords [label, "-", s]]
