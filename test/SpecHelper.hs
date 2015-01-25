{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( module X
                  , debug
                  , shouldReturn
                  , sequenceTests
                  , cleanPids
                  , pidFiles) where

import           Control.Applicative as X
import           Control.Concurrent.STM as X
import qualified Control.Concurrent.MSem as MSem
import           Control.Error as X hiding ((??))
import           Control.Lens as X hiding (elements)
import           Control.Monad.Free as X
import           Control.Monad.Reader.Class as X
import           Control.Monad.State.Strict as X
import           Control.Monad.Writer.Strict as X
import qualified Data.Map as X (Map)
import           Data.Maybe as X
import           Debug.Trace as X
import           Seraph.Types as X
import           System.IO.Error
import           System.Posix.Files
import           System.Posix.Process as X
import           System.Posix.Signals as X
import           System.Posix.Types as X
import           System.Posix.User as X hiding (groupName, userName)
import           Test.Tasty as X
import           Test.Tasty.HUnit as X
import           Test.Tasty.QuickCheck as X

import           Data.DeriveTH (derive)
import           Data.Derive.Arbitrary (makeArbitrary)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
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


instance Arbitrary Event where
  arbitrary = oneof [ NewConfig <$> arbitrary
                    , ProcessDeath <$> arbitrary
                    , ProgRunning <$> arbitrary
                    , ProgNotStarted <$> arbitrary <*> arbitrary
                    , pure ShutdownRequested
                    ]

instance Arbitrary Directive where
  arbitrary = oneof [ SpawnProg <$> arbitrary
                    , KillProg <$> arbitrary
                    ]

instance Arbitrary Directives where
  arbitrary = oneof [ Directives <$> arbitrary
                    , FinalDirectives <$> arbitrary
                    ]

debug :: Show a => a -> a
debug x = traceShow x x

shouldReturn :: (Show a, Eq a) => IO a -> a -> Assertion
shouldReturn action expected = (@?= expected) =<< action

-- ehhhhh
sequenceTests :: ((IO a -> IO a) -> TestTree) -> TestTree
sequenceTests f = withResource (MSem.new 1) (const noop) wrapTree
  where
    wrapTree :: IO (MSem.MSem Int) -> TestTree
    wrapTree semInIO = f $ \action -> do --todo: applicative
      sem <- semInIO
      MSem.with sem action

noop :: Monad m => m ()
noop = return ()

cleanPids = mapM_ (tryIOError . removeLink) pidFiles

pidFiles :: [FilePath]
pidFiles = ["test/jobs/pushover.pid", "test/jobs/hardboiled.pid"]
