{-# LANGUAGE ScopedTypeVariables #-}
module Seraph.CoreSpec
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception.Base
import           Seraph.Core
import           System.IO.Error
import           System.Timeout         (timeout)
-------------------------------------------------------------------------------
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Seraph.Core" [processDirectivesTests]


-------------------------------------------------------------------------------
processDirectivesTests :: TestTree
processDirectivesTests = sequenceTests $ \sync ->
  let syncWithCleanup = sync . withCleanup
  in testGroup "processDirectives" [

    testCase  "it runs a configured process" $ syncWithCleanup $ do
       vs <- buildEmptyState
       q  <- buildQueue
       let prog = pushover
       processDirectives vs q $ Directives [SpawnProg prog]
       Just pid <- getPid vs prog
       pidRunning pid `shouldReturn` True
  , testCase  "it kills a compliant process when necessary" $ syncWithCleanup $ do
       vs <- buildEmptyState
       q  <- buildQueue
       let prog = pushover
       processDirectives vs q $ Directives [SpawnProg prog]
       Just pid <- getPid vs prog
       processDirectives vs q $ Directives [KillProg (prog ^. name)]
       shouldStopRunning pid
  , testCase  "it kills a non-compliant process when necessary and configured with termGrace" $ syncWithCleanup $ do
       vs <- buildEmptyState
       q  <- buildQueue
       let prog = hardboiled
       processDirectives vs q $ Directives [SpawnProg prog]
       Just pid <- getPid vs prog
       processDirectives vs q $ Directives [KillProg (prog ^. name)]
       shouldStopRunning pid
  , testCase  "it spares a non-compliant process when not configured with termGrace" $ syncWithCleanup $ do
       vs <- buildEmptyState
       q  <- buildQueue
       let prog = hardboiledDontAbort
       processDirectives vs q $ Directives [SpawnProg prog]
       threadDelay 500000
       putStrLn "finished processing"
       Just pid <- getPid vs prog
       processDirectives vs q $ Directives [KillProg (prog ^. name)]
       shouldKeepRunning pid
    ]


-------------------------------------------------------------------------------
shouldKeepRunning pid = pidRunning pid `shouldEventuallyBe` True
shouldStopRunning pid = pidRunning pid `shouldEventuallyBe` False


-------------------------------------------------------------------------------
shouldEventuallyBe action value =
  checkTil (== value) action `shouldReturn` (Just value)


-------------------------------------------------------------------------------
checkTil :: (a -> Bool) -> IO a -> IO (Maybe a)
checkTil p x = timeout eventuallyInUs check
  where
    check = iterateUntil p x


-------------------------------------------------------------------------------
iterateUntil :: Monad m => (a -> Bool) -> m a -> m a
iterateUntil p x = do
  y <- x
  if p y
    then return y
    else iterateUntil p x


-------------------------------------------------------------------------------
eventuallyInUs = 3000000


-------------------------------------------------------------------------------
pidRunning pid = (isNothing <$> getProcessStatus False False pid) `catchIOError` handler
  where
--    handler :: SomeException -> IO Bool
    handler _ = return False

-------------------------------------------------------------------------------
buildEmptyState = atomically . newTVar $ ViewState mempty


-------------------------------------------------------------------------------
buildQueue      = newTQueueIO


-------------------------------------------------------------------------------
getPid vs prog = atomically (fmap (view pid) . findPid <$> readTVar vs)
  where
    findPid vs' = vs' ^. pHandles . at (prog ^. name)


-------------------------------------------------------------------------------
pushover = Program {
      _name       = (ProgramId "pushover")
    , _exec       = "test/jobs/pushover.sh"
    , _delay      = 0
    , _userName   = Nothing
    , _groupName  = Nothing
    , _stdout     = Just "test/jobs/pushover.stdout.log"
    , _stderr     = Just "test/jobs/pushover.stderr.log"
    , _workingDir = Nothing
    , _logExec    = Nothing
    , _pidFile    = Nothing
    , _env        = mempty
    , _termGrace  = Nothing
    }


-------------------------------------------------------------------------------
hardboiled = hardboiledDontAbort & termGrace .~ Just 1


-------------------------------------------------------------------------------
hardboiledDontAbort = pushover & name .~ (ProgramId "hardboiled")
                               & exec .~ "test/jobs/hardboiled.sh"
                               & stdout .~ Just "test/jobs/hardboiled.stdout.log"
                               & stderr .~ Just "test/jobs/hardboiled.stderr.log"


--TODO: write pids, reap them, then delete files
-------------------------------------------------------------------------------
withCleanup t = t `finally` cleanUp


-------------------------------------------------------------------------------
cleanUp = killPids >> cleanPids


-------------------------------------------------------------------------------
killPids = mapM_ (tryIOError . forceKill) . catMaybes =<< mapM loadPid pidFiles


-------------------------------------------------------------------------------
forceKill = signalProcess sigKILL


-------------------------------------------------------------------------------
loadPid :: FilePath -> IO (Maybe ProcessID)
loadPid f = (readMay <$> readFile f) `catchIOError` handler
  where
    handler _ = return Nothing
