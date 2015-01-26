{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seraph.Model
    ( Directive(..)
    , Event(..)
    , OracleM
    , oracle
    , oracleModel
    , oracleDebug
    ) where

-------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust, mapMaybe)
import           Data.Set                    (Set, (\\))
import qualified Data.Set                    as S
import           Debug.Trace
import           MVC
-------------------------------------------------------------------------------
import           Seraph.Types
-------------------------------------------------------------------------------


oracleModel :: Model Config Event (Directives, [String])
oracleModel = asPipe $ loop model
  where
    model = unwrapModel oracle
    -- model = unwrapModel oracleDebug


-------------------------------------------------------------------------------
type OracleM a = WriterT [String] (State Config) a


-------------------------------------------------------------------------------
unwrapModel :: (Event -> OracleM a)
            -> Event
            -> ListT (State Config) (a, [String])
unwrapModel f e = lift $ runWriterT (f e)


-------------------------------------------------------------------------------
oracleDebug :: Event -> OracleM Directives
oracleDebug e = do
  s <- get
  res <- traceShow s $ oracle e
  -- s' <- get
  -- traceShow ("EVT", e, "BEFORE", s, "AFTER", s') $ return res
  traceShow ("EVT", e, "RES", res) $ return res


-------------------------------------------------------------------------------
oracle :: Event -> OracleM Directives
oracle (ProcessDeath prid) = do
  progLogger "Process died"
  existing <- gets $ view $ running . at prid . to isJust
  mainLogger $ if existing
     then "Spawning " ++ prid ^. pidStr
     else "Process " ++ prid ^. pidStr ++ " not configured for restart"
  modify $ set (running . at prid) Nothing
  progs <- gets $ toListOf $ configured . ix prid
  return $ Directives $ if null progs
                        then  []
                        else map SpawnProg progs
  where
    progLogger = ctxLogger (prid ^. pidStr)
oracle (ProgRunning prid) = do
  modify $ \c -> c & running <>~ S.singleton prid
  progLogger "Marked as running"
  return $ Directives []
  where
    progLogger = ctxLogger (prid ^. pidStr)
oracle (NewConfig cfg) = do
  mainLogger "New config"
  oldCfg <- get
  put cfg
  let oldPids = configPids oldCfg
  let newPids = configPids cfg
  let currentlyRunning = oldCfg ^. running
  let spawnPids = newPids \\ oldPids
  let killPids = currentlyRunning \\ newPids
  spawnProgs <- getProgs $ spawnPids ^. to S.toList
  let spawns = map SpawnProg spawnProgs
  let kills  = map KillProg $ killPids ^. to S.toList
  mainLogger $ "Spawning " ++ slen spawns ++ " programs and killing " ++ slen kills
  return . Directives $ spawns ++ kills
oracle ShutdownRequested = do
  mainLogger "Shutdown requested."
  modify $ set configured mempty
  killPids <- gets $ view (running . to S.toList)
  return . FinalDirectives . map KillProg $ killPids
oracle (ProgNotStarted prid e) = do
  spawnProgs <- getProgs [prid]
  progLogger $ "Failed to start: " ++ show e ++ ", retrying."
  return $ Directives $ map SpawnProg spawnProgs
  where
    progLogger = ctxLogger (prid ^. pidStr)


-------------------------------------------------------------------------------
getProgs :: [ProgramId] -> OracleM [Program]
getProgs spawnPids = do
  cfg <- get
  return $ mapMaybe (progForPid cfg) spawnPids
  where
    progForPid cfg prid = cfg ^. configured . at prid


-------------------------------------------------------------------------------
configPids :: Config -> Set ProgramId
configPids cfg = cfg ^. configured . to M.keys . to S.fromList


-------------------------------------------------------------------------------
mainLogger :: MonadWriter [String] m => String -> m ()
mainLogger = ctxLogger "seraph"


-------------------------------------------------------------------------------
ctxLogger :: MonadWriter [String] m =>  String -> String -> m ()
ctxLogger label s = tell [unwords [label, "-", s]]


-------------------------------------------------------------------------------
slen :: Show a => [a] -> String
slen = show . length
