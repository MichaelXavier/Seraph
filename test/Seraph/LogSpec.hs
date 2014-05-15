module Seraph.LogSpec (tests) where

import SpecHelper

import Seraph.Types
import Seraph.Log

tests :: TestTree
tests = testGroup "Seraph.Log" [ testLoggerTests ]

testLoggerTests :: TestTree
testLoggerTests = testGroup "testLogger" [
    testCase "derp" $ runLogger
                                         ]

logger :: Logger (RWS (Logger m))
