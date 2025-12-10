module MF.Algorithms.MOP where

import           MiniC_lib


-- * Meet Over all Paths (MOP) Analysis

-- |A path is an ordered list of visited program labels.
type Path = [Label]


-- |Checks if a path is still allowed to visit a certain label.
nokloop :: Int -> Label -> Path -> Bool
nokloop k l p = count (== l) p <= k
 where
  count :: (a -> Bool) -> [a] -> Int
  count = (length .) . filter
