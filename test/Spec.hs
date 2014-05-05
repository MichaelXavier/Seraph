module Main (main) where

import Test.Tasty

import Seraph.ModelSpec

main :: IO ()
main = defaultMain $ testGroup "Seraph" [Seraph.ModelSpec.tests]
