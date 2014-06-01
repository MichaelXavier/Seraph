module Seraph.Util ( mRun
                   , concatMapM ) where

import Control.Monad (liftM)

-- Foldable for_
mRun :: Monad m => Maybe a -> (a -> m ()) ->  m ()
mRun m f = maybe (return ()) f m

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)
