module Seraph.Util (mRun) where

-- Foldable for_
mRun :: Monad m => (a -> m ()) -> Maybe a -> m ()
mRun = maybe (return ())
