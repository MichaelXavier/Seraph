{-# LANGUAGE TemplateHaskell #-}
module Seraph.Core (core) where

import Control.Lens
import Control.Applicative
import Control.Concurrent.STM
import Control.Error
import Data.Map (Map)
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import System.IO.Error (tryIOError)
import MVC
import System.Posix.Signals ( sigTERM
                            , sigINT
                            , sigHUP
                            , Handler(Catch)
                            , Signal
                            , installHandler
                            )

import Seraph.Config (load)
import Seraph.Process
import Seraph.Types
import Seraph.Util
import System.Exit (exitSuccess)

import Debug.Trace

data ViewState = ViewState { _pHandles :: Map ProgramId ProcessHandle }

makeClassy ''ViewState

core :: Config -> FilePath -> Managed (View ([Directive], [String]), Controller Event)
core initCfg fp = do
  exits <- exitController
  cfgC  <- configController initCfg fp
  (downstreamSide, ctrlSide) <- managed managedSpawn
  vs <- managed $ managedIO . atomically . newTVar $ ViewState mempty
  return (aggregatedView vs downstreamSide,
          exits <> downstreamController ctrlSide <> cfgC)

managedIO :: IO a -> (a -> IO x) -> IO x
managedIO = (>>=)

managedSpawn :: ((Output a, Input a) -> IO x) -> IO x
managedSpawn f = f =<< spawn Single

downstreamController :: Input DownstreamMsg -> Controller Event
downstreamController = asInput . fmap (\x -> traceShow ("YEAH",x) $  mapDownstream x)
  where
    mapDownstream (ProgStarted prid)          = ProgRunning prid
    mapDownstream (ProgEnded prid)            = ProcessDeath prid
    mapDownstream (ProgFailedToStart prid se) = ProgNotStarted prid se

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
                   | ProgFailedToStart ProgramId SpawnError deriving (Show)

type ViewM a = ReaderT (TVar ViewState) IO a

runViewM :: TVar ViewState -> ViewM a -> IO a
runViewM = flip runReaderT

aggregatedView :: TVar ViewState -> Output DownstreamMsg -> View ([Directive], [String])
aggregatedView vs out = handles _1 (processDirectives vs out) <>
                        handles _2 (processLogs vs)

processLogs :: TVar ViewState -> View [String]
processLogs vs = asSink $ runViewM vs . processLogs'

--TODO: use ViewM or drop it
processLogs' ::  [String] -> ViewM ()
processLogs' = mapM_ (liftIO . putStrLn)

--TODO break up with prisms if applicable
processDirectives :: TVar ViewState -> Output DownstreamMsg -> View [Directive]
processDirectives vs out = asSink $ runViewM vs . processDirectives' out

processDirectives' :: Output DownstreamMsg -> [Directive] -> ViewM ()
processDirectives' out = mapM_ (processDirective out)

--TODO: make sure we can't exit until we attempted kills on everything
processDirective :: Output DownstreamMsg -> Directive -> ViewM ()
processDirective out (SpawnProgs ps) = mapM_ (spawnProg' out) ps
processDirective out (KillProgs prids) = mapM_ (kill' out) prids
processDirective _   (Exit) = liftIO exitSuccess

spawnProg' :: Output DownstreamMsg -> Program -> ViewM ()
spawnProg' out prog = do
  result <- liftIO . normalizeExceptions . runSeraphProcessM . spawnProg $ prog
  liftIO $ print result
  either reportFailure registerHandle result
  where
--    reportFailure e = void . liftIO . atomically $ send out $ ProgFailedToStart prid e
    reportFailure e = do
      void . liftIO . atomically $ send out $ ProgFailedToStart prid e
    --DANGER: need good testing around this area. forgot to send out messages downstream
    registerHandle ph = do
      --FIXME: this blocks indefinitely
      void . liftIO . atomically $ send out $ ProgStarted prid
      writeHandle prid ph
      monitor out prid
    prid = prog ^. name
    normalizeExceptions = fmap (join . fmapL SpawnException) . tryIOError

writeHandle :: ProgramId -> ProcessHandle -> ViewM ()
writeHandle prid ph = do
  vsv <- ask
  liftIO . atomically $
    modifyTVar' vsv (\vs -> vs & pHandles . at prid .~ Just ph)


getHandle :: ProgramId -> ViewM (Maybe ProcessHandle)
getHandle prid = do
  vsv <- ask
  liftIO . atomically $
    view (pHandles . at prid) <$> readTVar vsv

monitor :: Output DownstreamMsg -> ProgramId -> ViewM ()
monitor out prid = do
  mph <- getHandle prid
  vsv <- ask
  mRun mph $ \ph -> void . liftIO . forkIO $ do
      runSeraphProcessM $ waitOn ph
      progEnded out prid vsv
      return ()

--FIXME: if we couldn't actually kill the program, should we send ended?
kill' :: Output DownstreamMsg -> ProgramId -> ViewM ()
kill' out prid = do
  vsv <- ask
  mph <- getHandle prid
  mRun mph $ \ph -> liftIO $ do
    runSeraphProcessM $ kill ph
    progEnded out prid vsv

--TODO: logging
progEnded :: Output DownstreamMsg -> ProgramId -> TVar ViewState -> IO ()
progEnded out prid vsv = void . atomically $ do
  modifyTVar' vsv $ \vs -> vs & pHandles . at prid .~ Nothing
  send out $ ProgEnded prid
