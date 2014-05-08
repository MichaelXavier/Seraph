{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seraph.Core (core) where

import Control.Lens
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent ( threadDelay
                          , forkIO)
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import MVC
import MVC.Prelude (producer)
import System.Posix.Signals ( sigTERM
                            , sigINT
                            , sigHUP
                            , Handler(Catch)
                            , Signal
                            , installHandler
                            )

import Seraph.Config (load)
import Seraph.Types
import System.Exit (exitSuccess)

--TODO: kill pipes to shutdown?

core :: Config -> FilePath -> Managed (View ([Directive], [String]), Controller Event)
core initCfg fp = do
  exits <- exitController
  cfgC  <- configController initCfg fp
  (downstreamSide, ctrlSide) <- managed managedSpawn
  return (aggregatedView downstreamSide,
          exits <> cfgC <> downstreamController ctrlSide)

managedSpawn :: ((Output a, Input a) -> IO x) -> IO x
managedSpawn f = f =<< spawn Single

downstreamController :: Input DownstreamMsg -> Controller Event
downstreamController = asInput . fmap mapDownstream
  where
    mapDownstream (ProgStarted pid) = ProgRunning pid
    mapDownstream (ProgEnded pid)   = ProcessDeath pid

data PHandle = PHandle
--TODO: real one
waitForProcess :: PHandle -> IO ()
waitForProcess = const $ threadDelay 2000000

exitController :: Managed (Controller Event)
exitController = managed $ \f -> do
  term <- exitSignalhandler sigTERM
  int  <- exitSignalhandler sigINT
  f $ asInput term <> asInput int

exitSignalhandler :: Signal -> IO (Input Event)
exitSignalhandler sig = do
  (output, input) <- spawn Single
  liftIO $ installHandler sig (Catch $ sendKill output) Nothing
  return input
  where
    sendKill output = void . atomically $ send output ShutdownRequested

--TODO: fire initial state somehow, lol
configController :: Config -> FilePath -> Managed (Controller Event)
configController initCfg fp = managed $ \f ->
  f . asInput =<< hupSignalHandler initCfg fp

hupSignalHandler :: Config -> FilePath -> IO (Input Event)
hupSignalHandler initCfg fp = do
  (output, input) <- spawn Single
  liftIO $ do
    installHandler sigHUP (Catch $ reloadConfig output) Nothing
    notify output initCfg
  return input
  where
    notify output = void . atomically . send output . NewConfig
    reloadConfig output = do
      putStrLn "HUP"
      cfg <- load fp
      case cfg of
        Right c -> putStrLn ("SENDING " ++ show cfg) >> notify output c
        _       -> error "TODO: handle error better"

data DownstreamMsg = ProgStarted ProgramId
                   | ProgEnded ProgramId

aggregatedView :: Output DownstreamMsg -> View ([Directive], [String])
aggregatedView out = (handles _1) (processDirectives out) <>
                     (handles _2) processLogs

processLogs :: View [String]
processLogs = asSink processLogs'

processLogs' :: [String] -> IO ()
processLogs' = mapM_ putStrLn

--TODO break up with prisms if applicable
processDirectives :: Output DownstreamMsg -> View [Directive]
processDirectives = asSink . processDirectives'

processDirectives' :: Output DownstreamMsg -> [Directive] -> IO ()
processDirectives' out = mapM_ (processDirective out)

--todo: signal
processDirective :: Output DownstreamMsg -> Directive -> IO ()
processDirective out (SpawnProgs ps) = mapM_ (spawnProg out) ps
processDirective out (KillProgs pids) = mapM_ (killProg out) pids
processDirective out (Exit) = liftIO exitSuccess

-- maybe async
spawnProg :: Output DownstreamMsg -> Program -> IO ()
spawnProg out prog = do
  putStrLn $ "START " ++ prog ^. name . to show
  notify (ProgStarted pid)
  void . forkIO $ waitForProcess PHandle >> notify (ProgEnded pid)
  where
    notify msg = void . atomically $ send out msg
    pid = prog ^. name

killProg :: Output DownstreamMsg -> ProgramId -> IO ()
killProg out pid = do
  putStrLn $ "KILL " ++ show pid
  void . atomically $ send out (ProgEnded pid)


-- create a ProcessRegistry that only exposes explicit actions
