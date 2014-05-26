{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (SomeException)
import Control.Error
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Monoid (mempty)
import System.Environment (getArgs)
import MVC (runMVC)
import System.Exit
import System.IO ( stderr
                 , hPutStrLn )

import Seraph.Config (load)
import Seraph.Core (core)
import Seraph.Model (oracleModel)

main :: IO ()
main = do
  initAndFp <- runEitherT $ do
    fp :: FilePath <- tryHead NoConfig =<< lift getArgs
    initCfg <- load' fp
    return (initCfg, fp)
  either bail (uncurry boot) initAndFp
  where
    boot initCfg fp = void $ runMVC mempty oracleModel (core initCfg fp)
    bail NoConfig = errorExit "Please pass a config"
    bail (InvalidConfig e) = errorExit ("Invalid config " ++ show e)
    load' fp = hoistEither . fmapL InvalidConfig =<< lift (load fp)

data BootError = NoConfig | InvalidConfig SomeException

errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg' >> exitFailure
  where
    msg' = "ERROR: " ++ msg
