{-# LANGUAGE ScopedTypeVariables #-}
module Seraph.Process ( waitOn
                      , kill
                      , spawnProg
                      , runSeraphChildM
                      , runSeraphProcessM ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Free (iterM)
import           Control.Monad.Trans
import           Data.Monoid
import           System.Exit (ExitCode(..))
import System.IO.Error ( tryIOError
                       , catchIOError
                       , isDoesNotExistError)
import qualified System.Posix.Directory as P
import qualified System.Posix.Env as P
import System.Posix.IO ( stdOutput
                       , stdError
                       , OpenFileFlags(..)
                       , defaultFileFlags
                       , OpenMode(WriteOnly) )
import qualified System.Posix.IO as P
import           System.Posix.Process (ProcessStatus(..))
import qualified System.Posix.Process as P
import System.Posix.Signals ( sigTERM
                            , sigKILL )
import qualified System.Posix.Signals as P
import System.Posix.Types ( UserID
                          , GroupID
                          , ProcessID )
import           System.Posix.User ( UserEntry(userID)
                         , GroupEntry(groupID) )
import qualified System.Posix.User as P

import           Seraph.Free
import           Seraph.Types
import           Seraph.Util

import           Debug.Trace
{-
spawning a program needs to handle these cases:

1. spawn logger if applicable
2. hook up file descriptors for logging
3. if logger dies first, log and retry?
4. caller needs to know when process is dead
5. caller needs to be able to prematurely kill the process

prerequisites: logging environment
output: some actions resulting in a ProcessHandle type
ProcessHandle will need to give a hook for death, and a command for kill

NemesisD, (dupTo stdout <target> >> hclose stdout ) IIRC
-}

--TODO: richer child error types
runSeraphChildM :: MonadIO m => SeraphChildM a -> m a
runSeraphChildM  = iterM run
  where
    run (SetUserID i next)              = liftIO' (P.setUserID i) >> next
    run (SetGroupID i next)             = liftIO' (P.setGroupID i) >> next
    run (ChangeWorkingDirectory p next) = liftIO' (P.changeWorkingDirectory p) >> next
    run (ExecuteFile f as e next)       = do
      existingEnv <- liftIO P.getEnvironment
      next =<< liftIO' (P.executeFile f searchPath as (Just (existingEnv <> e)))
    run (OpenFd p m fs next)            = next =<< liftIO' (P.openFd p m fmode fs)
    run (DupTo fd1 fd2 next)            = liftIO' (P.dupTo fd1 fd2) >> next
    fmode                               = Just 0o664
    searchPath                          = True
    liftIO' = liftIO . tryIOError

--FIXME: why would process status not be available. don't get the WNOHANG part
-- disabling blocking is doing weird thing: unclear if it actually waits for anything, triggers process death twice
runSeraphProcessM :: MonadIO m => SeraphProcessM a -> m a
runSeraphProcessM = iterM run
  where
    run (SignalProcess sig prid next)  = liftIO (P.signalProcess sig prid) >> next
    run (WaitSecs n next)              = liftIO (threadDelay (n * 1000000)) >> next
    run (GetUserEntryForName n next)   = next . fmap userID =<< liftTry (P.getUserEntryForName n)
    run (GetGroupEntryForName n next)  = next . fmap groupID  =<< liftTry (P.getGroupEntryForName n)
    run (ForkProcess m next)           = next =<< liftIO (P.forkProcess (void $ runSeraphChildM m))
    run (GetProcessStatus i next)      = next =<< liftIO (safeGetProcessStatus blockIfStillRunning stopped i)
    blockIfStillRunning = False
    stopped = False
    try' = fmap hush . tryIOError
    liftTry = liftIO . try'

safeGetProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
safeGetProcessStatus blockIfStillRunning stopped i = catchAlreadyGone $ P.getProcessStatus blockIfStillRunning stopped i
  where
    -- damn lies? what does nothing mean
    catchAlreadyGone x = x `catchIOError` catchIOErrorWith isDoesNotExistError (Just (Exited ExitSuccess))

kill :: ProcessHandle -> SeraphProcessM ()
kill ph = do
  softKill
  case ph ^. policy of
    HardKill n -> waitSecs n >> hardKillIfRunning
    _          -> return ()
  where
    softKill = signalProcess sigTERM $ ph ^. pid
    hardKillIfRunning = do
      ps <- getProcessStatus (ph ^. pid)
      --TODO: testme
      when (isNothing ps) hardKill
    hardKill = signalProcess sigKILL $ ph ^. pid

waitOn :: ProcessHandle -> SeraphProcessM ProcessStatus
waitOn ph = do
  ps <- getProcessStatus (ph ^. pid)
  maybe (waitOn ph) return ps

spawnProg :: Program -> SeraphProcessM (Either SpawnError ProcessHandle)
spawnProg prog = runEitherT $ do
  (cmd, args) <- failWith InvalidExec $ prog ^. exec ^? cmdSplit
  uid <- getId progUid userName
  gid <- getId progGid groupName
  waitSecs $ prog ^. delay
  pd <- lift $ forkProcess $ do
    mRun uid setUserID -- can mRuns be handled in maybeT that doesn't abort early?
    mRun gid setGroupID
    mRun (prog ^. workingDir) changeWorkingDirectory
    configureFDs prog
    void $ executeFile cmd args (prog ^. env)
  -- TODO: this fixes the test failure and i don't know why
  return $ ProcessHandle pd $ prog ^. to killPolicy
  where
    getId f reader = case prog ^. reader of
      Nothing -> return Nothing
      Just n -> hoistEither . fmap Just =<< lift (f n)

configureFDs :: Program -> SeraphChildM ()
configureFDs prog
  | logProg  = undefined --TODO
  | otherwise = do
    logToFile stdOutput $ fromMaybe devNull (prog ^. stdout)
    logToFile stdError $ fromMaybe devNull (prog ^. stderr)
  where
    logProg  = prog ^. logExec . to isJust
    logToFile existingFd fp = do
      --TODO handle errors appropriately
      Right targetFd <- openFd fp WriteOnly defaultFileFlags { append = True }
      void $ dupTo targetFd existingFd
    devNull = "/dev/null"

progUid :: String -> SeraphProcessM (Either SpawnError UserID)
progUid uname = extract <$> getUserEntryForName uname
  where
    extract = note InvalidUser

progGid :: String -> SeraphProcessM (Either SpawnError GroupID)
progGid gname = extract <$> getGroupEntryForName gname
  where
    extract = note InvalidGroup

cmdSplit :: Simple Prism String (String, [String])
cmdSplit = prism joinParts splitParts
  where
    joinParts (s, as) = unwords (s:as)
    splitParts str = case words str of
                       (s:as) -> Right (s, as)
                       _      -> Left str

catchIOErrorWith :: (IOError -> Bool) -> a -> IOError -> IO a
catchIOErrorWith p v e | p e       = return v
                       | otherwise = ioError e
