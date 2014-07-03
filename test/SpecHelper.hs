{-# LANGUAGE TemplateHaskell #-}
module SpecHelper ( module X
                  , debug ) where

import qualified Data.Map as X (Map)
import Debug.Trace as X
import Control.Applicative as X
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X
import Control.Lens as X hiding (elements)
import Control.Monad.Free as X
import Control.Monad.Reader.Class as X
import Control.Monad.State.Strict as X
import Control.Monad.Writer.Strict as X
import Data.Maybe as X
import Seraph.Types as X
import System.Posix.Signals as X
import System.Posix.User as X hiding (groupName, userName)
import System.Posix.Process as X
import System.Posix.Types as X

import Data.DeriveTH (derive)
import Data.Derive.Arbitrary (makeArbitrary)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = M.fromList <$> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = S.fromList <$> arbitrary

$(derive makeArbitrary ''ProgramId)
$(derive makeArbitrary ''Program)
$(derive makeArbitrary ''Config)

instance Arbitrary SpawnError where
  arbitrary = oneof [ pure InvalidExec
                    , pure InvalidUser
                    , pure InvalidGroup
                    , pure $ SpawnException undefined]-- whats an IOError?

debug :: Show a => a -> a
debug x = traceShow x x
