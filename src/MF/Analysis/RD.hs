{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module MF.Analysis.RD
  ( mfRD
  , RD(..)
  )
where

import           Prelude
import           MiniC_lib
import           MF.Evaluating
import           Base
import           MF.Analysis
import           MF.FreeNames                   ( freeNames )


import           Text.Printf                    ( printf )
import qualified Data.Set                      as Set
import           Data.Either                    ( fromRight )

-- * Reached-Definitions Type

initMF :: Program -> MF (Set.Set RD)
initMF prog = getElements
  prog
  framework
    { getI = Set.map (\x -> RD x (Label Nothing)) (freeNames prog)
    , getL = Lattice { join    = Set.union
                     , refines = flip Set.isProperSubsetOf
                     , bottom  = Set.empty
                     }
    }

-- * Monotone Framework

-- |Monotone Framework for Reached-Definition Analysis.
mfRD :: ValuesTable -> Program -> MF (Set.Set RD)
mfRD vt prog =
  forwards prog . distributive (killRD vt) (genRD vt) . initMF $ prog

 -- later replace with Block instead of  Statement
 -- attached ValuesTable to evaluate record indexes
killRD :: ValuesTable -> Statement -> Set.Set RD -> Set.Set RD
killRD vt (AssRecord label identifier aexpr1 aexpr2) bot =
  Set.insert (RD record_11 (Label Nothing)) newSet
    <> Set.insert (RD record_i2 (Label Nothing)) newSet
 where
  newSet    = Set.filter (\(RD x _) -> x == record_11 || x == record_i2) bot
  record_11 = LitRecord identifier (First (fromRight 1000 (evalAE aexpr1 vt)))
  record_i2 = LitRecord identifier (Second (fromRight 1000 (evalAE aexpr2 vt)))
killRD vt (AssLit label litExpr aexpr) bot = Set.insert
  (RD litExpr (Label Nothing))
  (Set.filter (\(RD x' _) -> litExpr == x') bot)
killRD vt (Read label litExpr) bot = Set.insert
  (RD litExpr (Label Nothing))
  (Set.filter (\(RD x' _) -> litExpr == x') bot)
killRD _ _ _ = Set.empty

genRD :: ValuesTable -> Statement -> Set.Set RD
genRD vt (AssRecord label identifier aexpr1 aexpr2) =
  Set.singleton
      (RD (LitRecord identifier (First (fromRight 1000 (evalAE aexpr1 vt))))
          label
      )
    <> Set.singleton
         (RD
           (LitRecord identifier (Second (fromRight 1000 (evalAE aexpr2 vt))))
           label
         )
genRD _ (AssLit label litExpr aexpr) = Set.singleton (RD litExpr label)
genRD _ (Read label litExpr        ) = Set.singleton (RD litExpr label)
genRD _ _                            = Set.empty



