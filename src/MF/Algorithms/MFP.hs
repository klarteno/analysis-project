
module MF.Algorithms.MFP where

import           Base
import           MiniC_lib

import           MF.Flowable
import           MF.Analysis

import           Control.Applicative            ( (<$>)
                                                , (<|>)
                                                )

import           Data.Maybe                     ( listToMaybe )
import qualified Data.List                     as List
import qualified Data.Set                      as Set

import           Debug.Trace                    ( trace )
import           Text.Printf                    ( printf )

-- * Maximal Fixed-Point (MFP) Analysis
-- |Performs pointwise maximal fixed-point analysis (as @mfp'@), using call
--  stacks as a context and @maxBound@ as a limit for the call stack size.
--  After the analysis it performs a join over all the results at a label.

-- * Contexts

-- |A Context object contains a number of analyses for different
--  contexts (e.g. call stacks), together with a list of the contexts
--  that were observed during the analysis.
data Context c a = Context { used :: [c] , at :: c -> a }

instance (Show c, Show a) => Show (Context c a) where
  show cxt = printf "{ %s}" show''
   where
    show'' = unlines $ List.intersperse ", " $ map show' $ used cxt
    show' c = printf "%s -> %s" (show c) (show $ cxt `at` c)

instance Functor (Context c) where
  fmap f c = c { at = f . at c }


-- |A call stack is a useful kind of context, which stores the procedure
--  calls in an analysis on a stack (with the top element being the most
--  recent procedure call).
type CallStack = [Label]

-- |A variant of cons `(::)` that returns a list of at most length `k`.
kcons :: Int -> a -> [a] -> [a]
kcons 0 _ _        = []
kcons _ x []       = [x]
kcons k x (y : ys) = x : kcons (k - 1) y ys


-- * Lifted Monotone Framework Operations
-- |Lifts a lattice on a property space to a lattice on a
--  context-sensitive property space.
getL' :: (Eq c) => MF a -> Lattice (Context c a)
getL' mf = Lattice { join = join', refines = refines', bottom = bottom' }
 where
  -- join: combine all used contexts, join pointwise
  join' c1 c2 = Context { used = union_used c1 c2, at = pointwise join c1 c2 }

  -- refines: check if c1 refines c2 over all used contexts
  refines' c1 c2 = List.any (pointwise refines c1 c2) (union_used c1 c2)

  -- bottom: without used contexts, always return bottom
  bottom' = Context { used = [], at = const (bottom (getL mf)) }

  -- utils: applies a lattice operation pointwise, unions the used contexts
  pointwise f c1 c2 c = f (getL mf) (c1 `at` c) (c2 `at` c)
  union_used c1 c2 = used c1 `List.union` used c2

-- |Computes the join over all results in a context, effectively flattening
--  the contexts.
joinall' :: Lattice a -> Context c a -> a
joinall' lat cxt = joinall lat $ map (cxt `at`) (used cxt)

-- |Computes the extremal value of an MF based on the callstack
--  in the context.
getI' :: MF a -> Context CallStack a
getI' mf = Context
  { used = [[]]
  , at   = \c -> case c of
             [] -> getI mf
             cs -> bottom (getL mf)
  }

-- |Computes a lifted transfer function that works pointwise.
getT' :: MF a -> Transfer (Context CallStack a) -- type Transfer a = Statement -> a -> a
getT' mf s ca = getT mf s <$> ca
