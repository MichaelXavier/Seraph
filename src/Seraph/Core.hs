{-# LANGUAGE TemplateHaskell #-}
module Seraph.Core
    ( core
      -- * Exported for testing
    , processDirectives
    , ViewState(..)
    , HasViewState(..)
    , DownstreamMsg(..)
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Map                 (Map)
import           Data.Monoid
import           MVC
import           MVC.Prelude              (producer)
import qualified Pipes.Prelude            as PP
import           System.IO.Error          (tryIOError)
import           System.Posix.Signals     (Handler (Catch), Signal,
                                           installHandler, sigHUP, sigINT,
                                           sigTERM)
-------------------------------------------------------------------------------
import           Seraph.Config            (load)
import           Seraph.Log
import           Seraph.Process
import           Seraph.Types
import           Seraph.Util
import           System.Exit              (exitSuccess)
-------------------------------------------------------------------------------


data ViewState = ViewState { _pHandles :: Map ProgramId ProcessHandle }

makeClassy ''ViewState


-------------------------------------------------------------------------------
core :: Config -> FilePath -> Managed (View (Directives, [String]), Controller Event)
core initCfg fp = do
  exits <- exitController
  cfgC  <- configController initCfg fp
  queue <- managed managedSpawn
  vs <- managed $ managedIO . atomically . newTVar $ ViewState mempty
  downstreamCtrl <- downstreamController queue
  return (aggregatedView vs queue,
          exits <> downstreamCtrl <> cfgC)


-------------------------------------------------------------------------------
managedIO :: IO a -> (a -> IO x) -> IO x
managedIO = (>>=)


-------------------------------------------------------------------------------
managedSpawn :: (TQueue a -> IO x) -> IO x
managedSpawn f = f =<< newTQueueIO


-------------------------------------------------------------------------------
downstreamController :: TQueue DownstreamMsg -> Managed (Controller Event)
downstreamController out = fmap mapDownstream <$> asInput' out
  where
    mapDownstream (ProgStarted prid)          = ProgRunning prid
    mapDownstream (ProgEnded prid)            = ProcessDeath prid
    mapDownstream (ProgFailedToStart prid se) = ProgNotStarted prid se


-------------------------------------------------------------------------------
asInput' :: TQueue a -> Managed (Controller a)
asInput' out = producer Single prod
  where
    prod = PP.repeatM . atomically . readTQueue $ out


-------------------------------------------------------------------------------
exitController :: Managed (Controller Event)
exitController = managed $ \f -> do
  term <- exitSignalhandler sigTERM
  int  <- exitSignalhandler sigINT
  f $ asInput term <> asInput int


-------------------------------------------------------------------------------
exitSignalhandler :: Signal -> IO (Input Event)
exitSignalhandler sig = do
  (output, input) <- spawn Single
  liftIO $ installHandler sig (Catch $ sendKill output) Nothing
  return input
  where
    sendKill output = void . atomically $ send output ShutdownRequested


-------------------------------------------------------------------------------
configController :: Config -> FilePath -> Managed (Controller Event)
configController initCfg fp = managed $ \f ->
  f . asInput =<< hupSignalHandler initCfg fp


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
data DownstreamMsg = ProgStarted ProgramId
                   | ProgEnded ProgramId
                   | ProgFailedToStart ProgramId SpawnError deriving (Show)


-------------------------------------------------------------------------------
type ViewM a = ReaderT (TVar ViewState) IO a


-------------------------------------------------------------------------------
runViewM :: TVar ViewState -> ViewM a -> IO a
runViewM = flip runReaderT


-------------------------------------------------------------------------------
-- Can't use handles here because we want to log first before
-- something that could terminate the progra
aggregatedView :: TVar ViewState -> TQueue DownstreamMsg -> View (Directives, [String])
aggregatedView vs out = asSink $ \(ds, ls) -> do
  processLogs ls
  processDirectives vs out ds


-------------------------------------------------------------------------------
--TODO: use ViewM or drop it
processLogs ::  [String] -> IO ()
processLogs ss = do
  time <- getFormattedTime
  mapM_ (writeLog time) ss
  where
    writeLog time s = putStrLn $ mconcat ["[", time, "] ", s]


-------------------------------------------------------------------------------
processDirectives :: TVar ViewState -> TQueue DownstreamMsg -> Directives -> IO ()
processDirectives vs out = void . launch
  where
    launch (Directives ds)      = launchMany ds
    launch (FinalDirectives ds) = launchMany ds >> liftIO exitSuccess
    launchMany                  = mapConcurrently (runViewM vs . launchOne)
    launchOne (SpawnProg p)     = spawnProg' out p
    launchOne (KillProg prid)   = kill' out prid


-------------------------------------------------------------------------------
spawnProg' :: TQueue DownstreamMsg -> Program -> ViewM ()
spawnProg' out prog = do
  result <- liftIO . normalizeExceptions . runSeraphProcessM . spawnProg $ prog
  either reportFailure registerHandle result
  where
    reportFailure e = void . liftIO . atomically $ writeTQueue out $ ProgFailedToStart prid e
    --DANGER: need good testing around this area. forgot to send out messages downstream
    registerHandle ph = do
      vsv <- ask
      void . liftIO . atomically $ do
        writeTQueue out $ ProgStarted prid
        writeHandle prid ph vsv
      monitor out prid
    prid = prog ^. name
    normalizeExceptions = fmap (join . fmapL SpawnException) . tryIOError


-------------------------------------------------------------------------------
writeHandle :: ProgramId -> ProcessHandle -> TVar ViewState -> STM ()
writeHandle prid ph vsv = modifyTVar' vsv (\vs -> vs & pHandles . at prid .~ Just ph)


-------------------------------------------------------------------------------
getHandle :: ProgramId -> ViewM (Maybe ProcessHandle)
getHandle prid = do
  vsv <- ask
  liftIO . atomically $
    view (pHandles . at prid) <$> readTVar vsv


-------------------------------------------------------------------------------
monitor :: TQueue DownstreamMsg -> ProgramId -> ViewM ()
monitor out prid = do
  mph <- getHandle prid
  vsv <- ask
  mRun mph $ \ph -> void . liftIO . forkIO . void $ do
    runSeraphProcessM . waitOn $ ph
    progEnded out prid vsv


-------------------------------------------------------------------------------
--FIXME: if we couldn't actually kill the program, should we send ended?
kill' :: TQueue DownstreamMsg -> ProgramId -> ViewM ()
kill' _ prid = do
  mph <- getHandle prid
  mRun mph $ \ph -> liftIO $
    runSeraphProcessM $ kill ph


-------------------------------------------------------------------------------
--TODO: logging
progEnded :: TQueue DownstreamMsg -> ProgramId -> TVar ViewState -> IO ()
progEnded out prid vsv = void . atomically $ do
  modifyTVar' vsv $ \vs -> vs & pHandles . at prid .~ Nothing
  writeTQueue out $ ProgEnded prid
