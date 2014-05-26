{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Seraph.ProcessSpec (tests) where

import qualified Data.Map as M

import Seraph.Free
import Seraph.Process

import SpecHelper

data Toughness = PushOver | HardBoiled deriving (Show, Eq)

data ProcState = ProcState { _runningProgs :: Map ProcessID Toughness
                           , _statuses     :: Map ProcessID ProcessStatus
                           , _uids         :: Map String UserID
                           , _gids         :: Map String GroupID
                           , _delays       :: [Int] } deriving (Show, Eq)

makeClassy ''ProcState

runSeraphProcessTest' :: ProcState -> SeraphProcessM a -> ProcState
runSeraphProcessTest' ps m = execState (runSeraphProcessTest m) ps

runSeraphProcessTest :: MonadState ProcState m => SeraphProcessM a -> m a
runSeraphProcessTest = iterM run
  where
    run (WaitSecs n next) = modify (\s -> s & delays  <>~ [n]) >> next
    run (SignalProcess sig pid next) = modify (simulateSignal sig pid) >> next
    run (GetUserEntryForName n f) = f =<< gets (view (uids . at n))
    run (GetGroupEntryForName n f) = f =<< gets (view (gids . at n))
    run (GetProcessStatus pid f) = f =<< gets (view (statuses . at pid))

simulateSignal :: Signal -> ProcessID -> ProcState -> ProcState
simulateSignal sig pid ps
  | isPoliteKill = politeKill sig pid ps
  | isHardKill   = hardKill pid ps
  | otherwise    = ps
  where isPoliteKill = sig == sigINT || sig == sigTERM
        isHardKill   = sig == sigKILL

politeKill :: Signal -> ProcessID -> ProcState -> ProcState
politeKill sig pid ps = case ps ^. runningProgs . at pid of
  Just PushOver -> ps & runningProgs . at pid .~ Nothing
                      & statuses . at pid .~ Just (Terminated sig False)
  _             -> ps

hardKill :: ProcessID -> ProcState -> ProcState
hardKill pid ps = ps & runningProgs . at pid .~ Nothing
                     & statuses . at pid .~ Just (Terminated sigKILL False)

-- tests
tests :: TestTree
tests = testGroup "Seraph.Process" [killTests]

--TODO: figure out behavior when pid is bogus
killTests :: TestTree
killTests = testGroup "kill" [
    testCase "killing pushover softly" $ do
       let penv' = runSeraphProcessTest' penv (kill pushoverPh)
       (penv' ^. runningProgs . at pushoverPid) @?= Nothing
       (penv' ^. statuses . at pushoverPid) @?= Just (Terminated sigTERM False)
       (penv' ^. delays) @?= []
  ,  testCase "killing hard boiled softly" $ do
       let penv' = runSeraphProcessTest' penv (kill $ hardPh & policy .~ SoftKill)
       assertBool "still running" $ isJust (penv' ^. runningProgs . at hardPid)
       (penv' ^. statuses . at hardPid) @?= Nothing
       (penv' ^. delays) @?= []
  ,  testCase "killing hard boiled with prejudice" $ do
       let penv' = runSeraphProcessTest' penv (kill hardPh)
       (penv' ^. runningProgs . at hardPid) @?= Nothing
       (penv' ^. statuses . at hardPid) @?= Just (Terminated sigKILL False)
       (penv' ^. delays) @?= [10]
                             ]

pushoverPid :: ProcessID
pushoverPid = 123

hardPid :: ProcessID
hardPid = 456

pushoverPh :: ProcessHandle
pushoverPh = ProcessHandle pushoverPid SoftKill

hardPh :: ProcessHandle
hardPh = ProcessHandle hardPid (HardKill 10)

penv :: ProcState
penv = ProcState (M.fromList [(pushoverPid, PushOver), (hardPid, HardBoiled)])
                 mempty
                 (M.fromList [("user1", CUid 1), ("user2", CUid 2)])
                 (M.fromList [("group1", CGid 1), ("group2", CGid 2)])
                 mempty
