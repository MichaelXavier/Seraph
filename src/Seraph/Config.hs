module Seraph.Config (load) where

import Control.Lens
import Control.Exception (SomeException)
import Data.Monoid

import Seraph.Types

--TODO
load :: FilePath -> IO (Either SomeException Config)
load _ = return $ Right cfg
  where
    cfg = mempty & configured . at pid .~ Just prog
    pid = ProgramId "example"
    prog = Program pid "do-stuff" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty Nothing
