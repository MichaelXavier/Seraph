module Seraph.Log (Logger) where

data Logger m = Logger { tag       :: String
                       , logAction :: String -> m () }
