{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Seraph.Process ( SpawnError(..)
                      ,  ProcessHandle
                      , waitOn
                      , kill
                      , spawnProg ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Error
import System.IO.Error (tryIOError)
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.Posix.User hiding ( userName
                                , groupName)

import Seraph.Types
import Seraph.Util
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

data SpawnError = InvalidExec
                | InvalidUser
                | InvalidGroup

data KillPolicy = SoftKill | HardKill Int

data ProcessHandle = ProcessHandle { _pid    :: ProcessID
                                   , _policy :: KillPolicy }

makeClassy ''ProcessHandle

waitOn :: ProcessHandle -> IO ProcessStatus
waitOn ph = do
  ps <- getProcessStatus blocking stopped (ph ^. pid)
  maybe (waitOn ph) return ps
  where
    blocking = True
    stopped  = True

kill :: ProcessHandle -> IO ()
kill ph = do
  softKill
  case ph ^. policy of
    HardKill n -> sleep n >> hardKill
    _          -> return ()
  where
    softKill = signalProcess sigTERM $ ph ^. pid
    hardKill = signalProcess sigKILL $ ph ^. pid
    sleep n  = threadDelay (n * 1000000)

spawnProg :: Program -> IO (Either SpawnError ProcessHandle)
spawnProg prog = runEitherT $ do
  (cmd, args) <- failWith InvalidExec $ prog ^. exec ^? cmdSplit
  uid <- getId progUid userName
  gid <- getId progGid groupName
  pd <- lift $ forkProcess $ do
    mRun setUserID uid -- can mRuns be handled in maybeT that doesn't abort early?
    mRun setGroupID gid
    mRun changeWorkingDirectory $ prog ^. workingDir
    configureFDs prog
    --TODO: hookup pipes
    --TODO: richer return type
    executeFile cmd searchPath args (prog ^. env . to Just)
  return $ ProcessHandle pd $ prog ^. to killPolicy
  where
    searchPath = True
    getId f reader = case prog ^. reader of
      Nothing -> return Nothing
      Just n -> hoistEither . fmap Just =<< lift (f n)

configureFDs :: Program -> IO ()
configureFDs prog
  | logProg  = undefined
  | otherwise = do
    logToFile stdOutput $ fromMaybe "/dev/null" (prog ^. stdout)
    logToFile stdError $ fromMaybe "/dev/null" (prog ^. stderr)
  where
    logProg  = prog ^. logExec . to isJust
    logToFile fd fp = do
      fd' <- openFd fp WriteOnly Nothing defaultFileFlags { append = True }
      void $ dupTo fd fd'

progUid :: String -> IO (Either SpawnError UserID)
progUid uname = extract <$> tryIOError (getUserEntryForName uname)
  where
    extract = swapLeft InvalidUser . fmap userID

progGid :: String -> IO (Either SpawnError GroupID)
progGid gname = extract <$> tryIOError (getGroupEntryForName gname)
  where
    extract = swapLeft InvalidGroup . fmap groupID

killPolicy :: Program -> KillPolicy
killPolicy Program { _termGrace = Just n } = HardKill n
killPolicy _                               = SoftKill

cmdSplit :: Simple Prism String (String, [String])
cmdSplit = prism joinParts splitParts
  where
    joinParts (s, as) = unwords (s:as)
    splitParts str = case words str of
                       (s:as) -> Right (s, as)
                       _      -> Left str


swapLeft :: c -> Either a b -> Either c b
swapLeft x = either (const $ Left x) Right
