module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.Exception.Base
import           Test.Tasty
-------------------------------------------------------------------------------
import           Seraph.CoreSpec
import           Seraph.ModelSpec
import           Seraph.ProcessSpec
import           Seraph.TypesSpec
import           SpecHelper
-------------------------------------------------------------------------------


main :: IO ()
main = run `finally` cleanup
  where
    run = defaultMain $ testGroup "Seraph"
      [ Seraph.ModelSpec.tests
      , Seraph.CoreSpec.tests
      , Seraph.ProcessSpec.tests
      , Seraph.TypesSpec.tests
      ]
    cleanup = cleanPids
