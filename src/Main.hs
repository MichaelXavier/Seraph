{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Concurrent (threadDelay)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as M
import MVC
import MVC.Prelude
import System.Exit

main :: IO ()
main = print =<< runMVC cfg (asPipe $ loop (unwrapModel model)) viewAndController
  where
    ps = M.fromList [ ("one", Prog "one" 10)
                    , ("two", Prog "two" 30)
                    ]
    cfg = Config ps mempty

type ProgName = String

data Prog = Prog { name :: String
                 , duration :: Int
                 } deriving (Show, Eq)

data Config = Config { progs :: Map ProgName Prog
                     , running :: Map ProgName Prog
                     } deriving (Show, Eq)

data Directive = NoOp
               | KillProgs [Prog]
               | SpawnProgs [Prog]
               | Exit String deriving (Show, Eq)

data Event = NewConfig Config
           | Shutdown
           | DeadProcess ProgName deriving (Show, Eq)

viewAndController :: Managed (View (Directive, [String]), Controller Event)
viewAndController = (,) <$> pure (asSink view) <*> controller

controller :: Managed (Controller Event)
controller = producer Single dataSource

dataSource :: Producer Event IO ()
dataSource = forever $ do
  yield (DeadProcess "one")
  liftIO $ threadDelay 1000000
  yield (DeadProcess "two")
  liftIO $ threadDelay 2000000
  yield (DeadProcess "bogus")

unwrapModel :: (Event -> WriterT [String] (State s) a)
          -> Event
          -> ListT (State s) (a, [String])
unwrapModel f e = lift $ runWriterT (f e)

tellLog = tell . (:[])

model :: Event -> WriterT [String] (State Config) Directive
model (NewConfig cfg) = do
  put cfg --todo more stuff
  tellLog "TODO: spawn and kill"
  return NoOp
model Shutdown = do
  tellLog "Shutting Down"
  return $ Exit "peace"
model (DeadProcess n) = do
  modify (\cfg -> cfg { running = M.delete n (running cfg)})
  p <- M.lookup n <$> gets progs
  maybe (return NoOp) spawnEm p
  where
    spawnEm p = tellLog ("Respawning " ++ n) >> return (SpawnProgs [p])

view :: (Directive, [String]) -> IO ()
view (d, ls) = do
  case d of
    NoOp            -> putStrLn "NOOP"
    (KillProgs ps)  -> forM_ ps $ \p -> putStrLn ("Killing " ++ show p)
    (SpawnProgs ps) -> forM_ ps $ \p -> putStrLn ("Spawning " ++ show p)
    (Exit str)      -> putStrLn str >> exitSuccess
  mapM_ putLog ls

putLog :: String -> IO ()
putLog s = putStrLn $ "LOG: " ++ s
