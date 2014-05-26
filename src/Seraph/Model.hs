{-# LANGUAGE FlexibleContexts #-}
module Seraph.Model ( Directive(..)
                    , Event(..)
                    , oracle
                    , oracleModel
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

oracleDebug e = do
  s <- get
  res <- traceShow s $ oracle e
  s' <- get
  --traceShow ("EVT", e, "BEFORE", s, "AFTER", s') $ return res
  traceShow ("EVT", e, "RES", res) $ return res

oracle :: Event -> WriterT [String] (State Config) [Directive]
oracle (ProcessDeath pid) = do
  existing <- gets $ view $ running . at pid . to isJust
  mainLogger $ if existing
     then "Spawning " ++ pid ^. pidStr
     else "Unknown process " ++ pid ^. pidStr
  modify $ set (running . at pid) Nothing
  progs <- gets $ toListOf $ configured . ix pid
  return $ if null progs
           then []
           else [SpawnProgs progs]
oracle (ProgRunning pid) = do
  modify $ \c -> c & running <>~ S.singleton pid
  mainLogger $ "Marked " ++ show pid ++ " as running"
  return []
oracle (NewConfig cfg) = do
  oldCfg <- get
  let oldPids = configPids oldCfg
  let newPids = configPids cfg
  let currentlyRunning = oldCfg ^. running
  let spawnPids = newPids \\ oldPids
  let killPids = currentlyRunning \\ newPids
  let spawnProgs = mapMaybe (\pid -> cfg ^. configured . at pid) (spawnPids ^. to S.toList)
  put cfg
  return [SpawnProgs spawnProgs, KillProgs (killPids ^. to S.toList)]
oracle ShutdownRequested = do
  modify $ set configured mempty
  killPids <- gets $ view (running . to S.toList)
  return [KillProgs killPids , Exit]

configPids :: Config -> Set ProgramId
configPids cfg = cfg ^. configured . to M.keys . to S.fromList

mainLogger :: MonadWriter [String] m => String -> m ()
mainLogger = ctxLogger "seraph"

ctxLogger :: MonadWriter [String] m =>  String -> String -> m ()
ctxLogger label s = tell [unwords [label, "-", s]]
