module Main (main) where

import Test.Tasty

import Seraph.CoreSpec
import Seraph.ModelSpec
import Seraph.ProcessSpec

main :: IO ()
main = defaultMain $ testGroup "Seraph" [
    Seraph.ModelSpec.tests
  , Seraph.CoreSpec.tests
  , Seraph.ProcessSpec.tests
  ]
