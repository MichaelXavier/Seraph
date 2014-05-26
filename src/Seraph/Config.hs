module Seraph.Config (load) where

import Control.Lens
import Control.Exception (SomeException)
import Data.Monoid

import Seraph.Types

--TODO: actually parse
load :: FilePath -> IO (Either SomeException Config)
load _ = return $ Right cfg
  where
    cfg = mempty & configured . at prid .~ Just prog
    prid = ProgramId "example"
    prog = Program prid "sleep 3" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty Nothing
