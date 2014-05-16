{-# LANGUAGE ScopedTypeVariables #-}
module Seraph.Process ( waitOn
                      , kill
                      , spawnProg ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Error
import System.IO.Error (tryIOError)
-- import System.Posix.Directory
import System.Posix.IO ( stdOutput
                       , stdError
                       , OpenFileFlags(..)
                       , defaultFileFlags
                       , OpenMode(WriteOnly) )
import System.Posix.Process (ProcessStatus)
import System.Posix.Signals ( sigTERM
                            , sigKILL )
-- import System.Posix.Types
import System.Posix.Types ( UserID
                          , GroupID
                          , Fd )

import Seraph.Free
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


--TODO: check process before hardkill
kill :: ProcessHandle -> SeraphProcessM ()
kill ph = do
  softKill
  case ph ^. policy of
    HardKill n -> waitSecs n >> hardKill
    _          -> return ()
  where
    softKill = signalProcess sigTERM $ ph ^. pid
    hardKill = signalProcess sigKILL $ ph ^. pid

waitOn :: ProcessHandle -> SeraphProcessM ProcessStatus
waitOn ph = do
  ps <- getProcessStatus blocking stopped (ph ^. pid)
  maybe (waitOn ph) return ps
  where
    blocking = True
    stopped  = True

{-
figure out how to embed these frees.

fizruk suggests FreeT f (Free g)

or data types ala cart a la Free (f :+: g) (compdata library)
http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

or write a function Bar m -> Foo m

3 options: FreeT f (Free g), Free (f :+: g), f ~> g
-}
spawnProg :: Program -> SeraphProcessM (Either SpawnError ProcessHandle)
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
    void $ executeFile cmd args (prog ^. env)
  return $ ProcessHandle pd $ prog ^. to killPolicy
  where
    getId f reader = case prog ^. reader of
      Nothing -> return Nothing
      Just n -> hoistEither . fmap Just =<< lift (f n)

configureFDs :: Program -> SeraphChildM ()
configureFDs prog
  | logProg  = undefined
  | otherwise = do
    logToFile stdOutput $ fromMaybe "/dev/null" (prog ^. stdout)
    logToFile stdError $ fromMaybe "/dev/null" (prog ^. stderr)
  where
    logProg  = prog ^. logExec . to isJust
    logToFile fd fp = do
      fd' <- openFd fp WriteOnly defaultFileFlags { append = True }
      void $ dupTo fd fd'

progUid :: String -> SeraphProcessM (Either SpawnError UserID)
progUid uname = extract <$> getUserEntryForName uname
  where
    extract = note InvalidUser

progGid :: String -> SeraphProcessM (Either SpawnError GroupID)
progGid gname = extract <$> getGroupEntryForName gname
  where
    extract = note InvalidGroup

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
