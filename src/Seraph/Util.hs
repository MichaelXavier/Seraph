module Seraph.Util (mRun) where

-- Foldable for_
mRun :: Monad m => Maybe a -> (a -> m ()) ->  m ()
mRun m f = maybe (return ()) f m
