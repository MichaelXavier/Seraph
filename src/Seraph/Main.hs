module Main (main) where

import Control.Monad (void)
import Data.Monoid (mempty)
import System.Environment (getArgs)
import MVC (runMVC)

import Seraph.Config (load)
import Seraph.Core (core)
import Seraph.Model (oracleModel)

main :: IO ()
main = do
  (fp:_) <- getArgs
  (Right initCfg) <- load fp
  void $ runMVC mempty oracleModel (core initCfg fp)
