{-# LANGUAGE TupleSections #-}
module MF.Algorithms.MFP_backup
  ( mfp
  , mfpk
  , mfp'
  , mfpk'
  )
where

import           Base
import           MiniC_lib

import           MF.Flowable
import           MF.Analysis


import qualified Data.List                     as L
import qualified Data.Set                      as S

import           Text.Printf                    ( printf )

-- * Maximal Fixed-Point (MFP) Analysis
-- |Performs pointwise maximal fixed-point analysis (as @mfp'@), using call
--  stacks as a context and @maxBound@ as a limit for the call stack size.
--  After the analysis it performs a join over all the results at a label.
mfp :: (Show a) => Algorithm a a
mfp mf l = joinall' (getL mf) (mfp' mf l)

-- |See @mfp@. Takes a value @k@ as the call stack limit.
mfpk :: (Show a) => Int -> Algorithm a a
mfpk k mf l = joinall' (getL mf) (mfpk' k mf l)

-- *  Fixed-Point  Analysis.

-- |See @mfp@. Returns the pointwise analysis instead of joining all results.
mfp' :: (Show a) => Algorithm a (Context CallStack a)
mfp' = mfpk' maxBound

-- |See @mfp@. Takes a value @k@ as the call stack limit, and returns the
--  pointwise analysis instead of joining all results.
mfpk' :: (Show a) => Int -> Algorithm a (Context CallStack a)
mfpk' k mf | isForwards mf  = mfpO mf
           | isBackwards mf = mfpI mf
 where
  mfpI mf = fixMFP k mf (mkWorkList mf) (mkAnalysis mf)
  mfpO mf l = let tr = applyT' mf l in tr (mfpI mf l)

-- |A worklist is a list of flows still to examine.
type WorkList = [Flow]

-- |Compute the maximal fixed point for an MF.
fixMFP
  :: (Show a)
  => Int
  -> MF a
  -> WorkList
  -> Analysis (Context CallStack a)
  -> Analysis (Context CallStack a)
fixMFP 0 _  _  _   = error "mfpk: k should be (>=0)"
fixMFP k mf [] mfp = mfp
fixMFP k mf (w : ws) mfp =
  let

-- import: refines as (<:), join as (\/) and k-cons as (!:)
      x <: y = refines (getL' mf) x y
      x \/ y = join (getL' mf) x y
      x !: xs = kcons k x xs
  in  case w of

-- intraprocedural analysis:
        Intra a b ->
          let

-- data: new analysis at l, old analysis at l'
              mfpA = applyT' mf a (mfp a)
              mfpB = mfp b

              -- recursive case: compute new worklist and analysis
              mfp' k = if k == b then mfpA \/ mfpB else mfp k
              ws' = filter (`flowsFrom` b) (mkWorkList mf) ++ ws
          in  if mfpA <: mfpB then fixMFP k mf ws' mfp' else fixMFP k mf ws mfp







-- |Initial worklist of the mfp algorithm.
mkWorkList :: MF a -> WorkList
mkWorkList mf = S.toList (getF mf)

-- |Initial output of the   Analysis algorithm.
mkAnalysis :: MF a -> Analysis (Context CallStack a)
mkAnalysis mf l | l `S.member` getE mf = getI' mf
                | otherwise            = bottom (getL' mf)

-- |Applies a transfer function for a nested block.
--  Note: this function is different from the @applyT@ defined in the MOP module.
applyT :: MF a -> Label -> a -> a
applyT mf l = getT mf (select l (S.map (\(BlockSt s) -> s) (getBlocks mf)))

-- * Contexts

-- |A Context object contains a number of analyses for different
--  contexts (e.g. call stacks), together with a list of the contexts
--  that were observed during the analysis.
data Context c a = Context { used :: [c] , at :: c -> a }

instance (Show c, Show a) => Show (Context c a) where
  show cxt = printf "{ %s}" show''
   where
    show'' = unlines $ L.intersperse ", " $ map show' $ used cxt
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
  refines' c1 c2 = L.any (pointwise refines c1 c2) (union_used c1 c2)

  -- bottom: without used contexts, always return bottom
  bottom' = Context { used = [], at = const (bottom (getL mf)) }

  -- utils: applies a lattice operation pointwise, unions the used contexts
  pointwise f c1 c2 c = f (getL mf) (c1 `at` c) (c2 `at` c)
  union_used c1 c2 = used c1 `L.union` used c2

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
getT' :: MF a -> Transfer (Context CallStack a)
getT' mf s ca = getT mf s <$> ca

-- |Applies a lifted transfer function that works pointwise.
applyT' :: MF a -> Label -> Context c a -> Context c a
applyT' mf l ca = applyT mf l <$> ca
