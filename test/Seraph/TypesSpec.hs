module Seraph.TypesSpec
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Test.QuickCheck.Property.Monoid
-------------------------------------------------------------------------------
import           Seraph.Types
import           SpecHelper
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Seraph.Types" [directivesTests]


-------------------------------------------------------------------------------
directivesTests :: TestTree
directivesTests = testGroup "Directives" [
    testProperty "Monoid" $ eq $ prop_Monoid (T :: T Directives)
  ]
