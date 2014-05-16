{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Seraph.ProcessSpec (tests) where

import Seraph.Free
import Seraph.Process

import SpecHelper

tests :: TestTree
tests = testGroup "Process" [killTests]

killTests :: TestTree
killTests = testGroup "kill" [
                             ]



data Toughness = PushOver | HardBoiled

data ProcState = ProcState { _runningProgs :: Map ProcessID Toughness
                           , _statuses     :: Map ProcessID ProcessStatus
                           , _uids         :: Map String UserID
                           , _gids         :: Map String GroupID }

makeClassy ''ProcState

--TODO: record delays
runSeraphProcessTest :: MonadState ProcState m => SeraphProcessM a -> m a
runSeraphProcessTest = iterM run
  where
    run (WaitSecs _ next) = next
    run (SignalProcess sig pid next) = undefined
    run (GetUserEntryForName n f) = f =<< gets (view (uids . at n))
    run (GetGroupEntryForName n f) = f =<< gets (view (gids . at n))
    run (GetProcessStatus pid f) = f =<< gets (view statuses . at pid)
