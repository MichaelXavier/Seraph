module Seraph.ModelSpec (tests) where

import qualified Data.Set as S
import SpecHelper

import Seraph.Model

tests :: TestTree
tests = testGroup "Seraph.Model" [oracleTests]

oracleTests :: TestTree
oracleTests = testGroup "oracle" [
    testProperty "ProcessDeath on unknown process" $ \pid cfg ->
      let cfg' = removeFromConfig cfg pid
          res  = runOracle (ProcessDeath pid) cfg'
      in res == ([], ["seraph - Unknown process " ++ pid ^. pidStr], cfg')
  , testProperty "Dead process never in running" $ \pid cfg ->
      let (_, _, cfg') = runOracle (ProcessDeath pid) cfg
      in isNothing (cfg' ^. running . at pid)
  , testProperty "Valid dead process gets spawned" $ \prog cfg ->
      let pid = prog ^. name
          cfg' = cfg & configured . at pid .~ Just prog
          (ds, _, _) = runOracle (ProcessDeath pid) cfg'
      in ds == [SpawnProgs [prog]]
  , testProperty "Prog running adds to running" $ \pid cfg ->
      let (_, _, cfg') = runOracle (ProgRunning pid) cfg
      in isJust (cfg' ^. running . at pid)
  , testProperty "Shutdown always ends in Exit" $ \cfg ->
      let (ds, _, _) = runOracle ShutdownRequested cfg
          finalCommand = lastOf traverse ds
      in finalCommand == Just Exit
  , testProperty "Shutdown always empties config" $ \cfg ->
      let (_, _, cfg') = runOracle ShutdownRequested cfg
      in cfg' ^. configured == mempty
  , testProperty "Shutdown kills all running" $ \cfg ->
     let (KillProgs pids:_, _, _) = runOracle ShutdownRequested cfg
      in S.fromList pids == (cfg ^. running)
  --TODO: given a reasonable (or empty) config, any list of operations will never result in mismapped ProgIds
                                 ]

removeFromConfig :: Config -> ProgramId -> Config
removeFromConfig cfg pid = cfg & configured . at pid .~ Nothing
                               & running . at pid .~ Nothing

runOracle :: Event -> Config -> ([Directive], [String], Config)
runOracle e cfg = (ds, ls, cfg')
  where
    ((ds, ls), cfg') = runState (runWriterT (oracle e)) cfg
