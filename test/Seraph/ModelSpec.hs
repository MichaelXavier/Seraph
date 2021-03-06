module Seraph.ModelSpec
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.Map            as M
import qualified Data.Set            as S
-------------------------------------------------------------------------------
import           Seraph.Model
import           Seraph.Types
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Seraph.Model" [oracleTests]


-------------------------------------------------------------------------------
oracleTests :: TestTree
oracleTests = testGroup "oracle" [
    testProperty "ProcessDeath on unknown process" $ \prid cfg ->
      let cfg' = removeFromConfig cfg prid
          res  = runOracle (ProcessDeath prid) cfg'
          diedMsg = (prid ^. pidStr) ++ " - Process died"
          unknownMsg = "seraph - Process " ++ prid ^. pidStr ++ " not configured for restart"
          expectedLogs = [diedMsg, unknownMsg]
      in res == (Directives [], expectedLogs, cfg')
  , testProperty "Dead process never in running" $ \prid cfg ->
      let (_, _, cfg') = runOracle (ProcessDeath prid) cfg
      in isNothing (cfg' ^. running . at prid)
  , testProperty "Valid dead process gets spawned" $ \prog cfg ->
      let prid = prog ^. name
          cfg' = cfg & configured . at prid .~ Just prog
          (ds, _, _) = runOracle (ProcessDeath prid) cfg'
      in ds == Directives [SpawnProg prog]
  , testProperty "Prog running adds to running" $ \prid cfg ->
      let (_, _, cfg') = runOracle (ProgRunning prid) cfg
      in isJust (cfg' ^. running . at prid)
  , testProperty "Shutdown is always a final set of directives" $ \cfg ->
      let (ds, _, _) = runOracle ShutdownRequested cfg
      in isFinal ds
  , testProperty "Shutdown always empties config" $ \cfg ->
      let (_, _, cfg') = runOracle ShutdownRequested cfg
      in cfg' ^. configured == mempty
  , testProperty "Shutdown kills all running" $ \cfg ->
     let (FinalDirectives ds, _, _) = runOracle ShutdownRequested cfg
         prids                 = map (\(KillProg prid) -> prid) ds
     in S.fromList prids == (cfg ^. running)
  , testProperty "ProgNotStarted only ever launches a max of 1 process" $ \prid e cfg ->
     let pns = ProgNotStarted prid e
         (Directives ds, _, _) = runOracle pns cfg
     in length ds <= 1 && all isSpawn ds
  , testProperty "any sequence of events exept a new config or shutdown will not modify configured programs" $ \cfg evts ->
     let evts' = filter (not . (isNewConfig <||> isShutdownRequested)) evts
         cfg' = execState (runWriterT (runEvents evts')) cfg
     in cfg' ^. configured === cfg ^. configured
  , testProperty "any sequence of events exept a new config will not launch a directive for a program not in the config" $ \cfg evts ->
     let evts' = filter (not . isNewConfig) evts
         ds = evalState (fst <$> runWriterT (runEvents evts')) cfg
         progs = S.fromList $ getProgs ds
         pids = S.fromList $ getPids ds
         origProgs = S.fromList $ M.elems (cfg ^. configured)
         origPids = S.fromList $ M.keys (cfg ^. configured) ++ S.toList (cfg ^. running)
     in progs === origProgs .&&. pids === origPids
  --TODO: given a reasonable (or empty) config, any list of operations will never result in mismapped ProgIds
  ]


-------------------------------------------------------------------------------
--TODO: lens
unDirectives :: Directives -> [Directive]
unDirectives (Directives ds) = ds
unDirectives (FinalDirectives ds) = ds


-------------------------------------------------------------------------------
--TODO: prism
getProgs :: Directives -> [Program]
getProgs = catMaybes . fmap go . unDirectives
  where
    go (SpawnProg p) = Just p
    go _ = Nothing


-------------------------------------------------------------------------------
getPids :: Directives -> [ProgramId]
getPids = catMaybes . fmap go . unDirectives
  where
    go (KillProg p) = Just p
    go _ = Nothing


-------------------------------------------------------------------------------
--TODO: use prisms
isSpawn :: Directive -> Bool
isSpawn (SpawnProg _) = True
isSpawn _             = False

-------------------------------------------------------------------------------
isFinal :: Directives -> Bool
isFinal (FinalDirectives _) = True
isFinal _                   = False


-------------------------------------------------------------------------------
isNewConfig :: Event -> Bool
isNewConfig (NewConfig _) = True
isNewConfig _             = False


-------------------------------------------------------------------------------
isShutdownRequested :: Event -> Bool
isShutdownRequested ShutdownRequested = True
isShutdownRequested _                 = False


-------------------------------------------------------------------------------
removeFromConfig :: Config -> ProgramId -> Config
removeFromConfig cfg prid = cfg & configured . at prid .~ Nothing
                               & running . at prid .~ Nothing


-------------------------------------------------------------------------------
runOracle :: Event -> Config -> (Directives, [String], Config)
runOracle e cfg = (ds, ls, cfg')
  where
    ((ds, ls), cfg') = runState (runWriterT (oracle e)) cfg


-------------------------------------------------------------------------------
runEvents :: [Event] -> OracleM Directives
runEvents evts = mconcat <$> mapM oracle evts


-------------------------------------------------------------------------------
(<||>) = liftA2 (||)
