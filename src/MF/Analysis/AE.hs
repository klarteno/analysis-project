module MF.Analysis.AE
  ( mfAE
  )
where

import           Base
import           MiniC_lib
import           MF.Analysis
import           MF.Available
import           MF.FreeNames

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import qualified Data.Foldable                 as S
                                                ( foldMap )

initMF prog = getElements
  prog
  framework
    { getI = S.empty
    , getL = Lattice { join    = S.intersection
                     , refines = S.isProperSubsetOf
                     , bottom  = getavailable prog
                     }
    }

-- * Monotone Framework Instance

-- |Monotone Framework for Available Expression Analysis.
mfAE :: Program -> MF (Set AExpr)
mfAE prog = forwards prog . distributive killAE genAE . initMF $ prog

killAE :: Statement -> Set AExpr -> Set AExpr
killAE (AssLit _ litexpr _) bot =
  S.filter (isFreeIn (getIdentifier litexpr)) bot
killAE (AssRecord _ name _ _) bot = S.filter (isFreeIn name) bot
killAE (IfElse _ st1 st2    ) bot = killAE st1 bot <> killAE st2 bot
killAE (If    _ st          ) bot = killAE st bot
killAE (While _ st          ) bot = killAE st bot
killAE (Skip _              ) _   = S.empty
killAE (Read _ litexpr) bot = S.filter (isFreeIn (getIdentifier litexpr)) bot
killAE (Write   _   litexpr ) _   = S.empty
killAE (StmtSeq st1 st2     ) bot = killAE st1 bot <> killAE st2 bot
killAE StEmpty                _   = S.empty


genAE :: Statement -> Set AExpr
genAE (AssLit _ litexpr aexpr) =
  S.filter (not . isFreeIn (getIdentifier litexpr)) (available aexpr)
genAE (AssRecord _ litexpr aexpr1 aexpr2) =
  S.filter (not . isFreeIn litexpr) (available aexpr1)
    <> S.filter (not . isFreeIn litexpr) (available aexpr2)
genAE (IfElse _ st1 st2   ) = genAE st1 <> genAE st2
genAE (If    _ st         ) = genAE st
genAE (While _ st         ) = genAE st
genAE (Skip _             ) = S.empty
genAE (Read    _   litexpr) = S.empty
genAE (Write   _   litexpr) = S.empty
genAE (StmtSeq st1 st2    ) = genAE st1 <> genAE st2
genAE StEmpty               = S.empty


getIdentifier :: LiteralExpr -> IdentifierName
getIdentifier (LitVar name     ) = name
getIdentifier (LitRecord name _) = name
getIdentifier (LitArray  name _) = name
