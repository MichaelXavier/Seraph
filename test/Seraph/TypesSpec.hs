module Seraph.TypesSpec
    ( tests
    ) where

import SpecHelper
import Test.QuickCheck.Property.Monoid

import Seraph.Types


tests :: TestTree
tests = testGroup "Seraph.Types" [directivesTests]


directivesTests :: TestTree
directivesTests = testGroup "Directives" [
    testProperty "Monoid" $ eq $ prop_Monoid (T :: T Directives)
                                         ]
