{-# LANGUAGE TypeSynonymInstances #-}
module Base where

import           Text.Printf                    ( printf )

import           Data.Monoid                    ( (<>) )
--import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Foldable                 as F
                                                ( foldMap )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.String

import           MiniC_lib

type FTable = Map.Map Name Declaration

mkFTable :: Declaration -> FTable
mkFTable d = Map.fromList (withName d)
 where
  withName d@(DeclVar label (LitVar (IdentifierName name))     ) = [(name, d)]
  withName d@(DeclArray label (IdentifierName name) (Size size)) = [(name, d)]
  withName d@(DeclRecord label (IdentifierName name) (First i1) (Second i2)) =
    [(name, d)]
  withName d@(DeclSeq d1 d2) = (++) (withName d1) (withName d2)


type Name = String

class LabelsOp a where
  getLabels   :: a -> Set.Set Label

instance LabelsOp Statement where
  getLabels (StmtSeq s1 s2        ) = getLabels s1 <> getLabels s2
--foldr (Set.union . getLabels) Set.empty xs
-- foldr Set.union Set.empty (foldr ((:) . getLabels) [] xs)
  getLabels (AssLit label _ _     ) = Set.singleton label
  getLabels (AssRecord label _ _ _) = Set.singleton label
  getLabels (IfElse bexpr st1 st2) =
    getLabels bexpr <> getLabels st1 <> getLabels st2
  getLabels (While bexpr st) = getLabels bexpr <> getLabels st
  getLabels (Skip label    ) = Set.singleton label
  getLabels (Read  label _ ) = Set.singleton label
  getLabels (Write label _ ) = Set.singleton label

instance LabelsOp BExpr where
  getLabels (BoolConst label _  ) = Set.singleton label
  getLabels (Not       label _  ) = Set.singleton label
  getLabels (BBinary label _ _ _) = Set.singleton label
  getLabels (RBinary label _ _ _) = Set.singleton label


select :: Label -> Set.Set Statement -> Statement
select label xs = isolated $ Set.elems (Set.filter hasLabel xs)
 where
  isolated :: [Statement] -> Statement
  isolated []  = error ("no statement with label " ++ show label)
  isolated [x] = x
  isolated _   = error ("multiple statements with label " ++ show label)

  hasLabel :: Statement -> Bool
  hasLabel x = getLabels x == Set.singleton label
