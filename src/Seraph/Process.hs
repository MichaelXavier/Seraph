{-# LANGUAGE ScopedTypeVariables #-}
module Seraph.Process ( SpawnError(..)
                      , spawnProg ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Error
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.Posix.User

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

--TODO: logger
spawnProg :: Program -> IO (Either SpawnError ProcessID)
spawnProg prog = runEitherT $ do
  (cmd, args) <- failWith InvalidExec $ prog ^. exec ^? cmdSplit
  uid <- hoistEither =<< lift (progUid prog)
  gid <- hoistEither =<< lift (progGid prog)
  lift $ forkProcess $ do
    mRun setUserID uid -- can mRuns be handled in maybeT that doesn't abort early?
    mRun setGroupID gid
    mRun changeWorkingDirectory $ prog ^. workingDir
    configureLogging prog
    --TODO: hookup pipes
    --TODO: richer return type
    executeFile cmd searchPath args (prog ^. env . to Just)
  where
    searchPath = True

configureLogging :: Program -> IO ()
configureLogging prog
  | logProg  = undefined
  | otherwise = do
    logToFile stdOutput $ fromMaybe "/dev/null" (prog ^. stdout)
    logToFile stdError $ fromMaybe "/dev/null" (prog ^. stderr)
  where
    logProg  = prog ^. logExec . to isJust
    logToFile fd fp = do
      fd' <- openFd fp WriteOnly Nothing defaultFileFlags { append = True }
      void $ dupTo fd fd'

progUid :: Program -> IO (Either SpawnError (Maybe UserID))
progUid Program { _userName = Nothing } = return $ Right Nothing
progUid Program { _userName = Just un } = undefined

progGid :: Program -> IO (Either SpawnError (Maybe GroupID))
progGid Program { _groupName = Nothing } = return $ Right Nothing
progGid Program { _groupName = Just un } = undefined

cmdSplit :: Simple Prism String (String, [String])
cmdSplit = prism joinParts splitParts
  where
    joinParts (s, as) = unwords (s:as)
    splitParts str = case words str of
                       (s:as) -> Right (s, as)
                       _      -> Left str

