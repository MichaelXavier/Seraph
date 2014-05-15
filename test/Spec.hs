module Main (main) where

import Test.Tasty

import Seraph.ModelSpec
import Seraph.LogSpec

main :: IO ()
main = defaultMain $ testGroup "Seraph" [ Seraph.ModelSpec.tests
                                        , Seraph.LogSpec.tests ]
