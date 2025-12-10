module MF.FreeNames where

import           MiniC_lib

import qualified Data.Set                      as Set

-- * Free Variable Names

class FreeNames a where
  freeNames :: a -> Set.Set LiteralExpr
  isFreeIn :: IdentifierName -> a -> Bool
  --isFreeIn name a = Set.member name (freeNames a)
  isFreeIn name a = Set.elemAt 0 (Set.map (isx name)  (freeNames a))
                                    where   isx name (LitVar ident) = name == ident
                                            isx name (LitRecord ident _) = name == ident
                                            isx name (LitArray ident _) = name == ident
instance FreeNames Program where
  freeNames (Dec d           ) = Set.empty
  freeNames (St  s           ) = freeNames s
  freeNames (ProgramSeq p1 p2) = freeNames p1 <> freeNames p2
    -- foldr ((<>) . freeNames) S.empty xs --foldl (<>) S.empty (map freeNames xs)

instance FreeNames Statement where
  freeNames (AssLit _ lit aexpr) = freeNames aexpr
  freeNames (AssRecord _ _ aexpr1 aexpr2) =
    freeNames aexpr1 <> freeNames aexpr2
  freeNames (IfElse _ s1 s2   ) = freeNames s1 <> freeNames s2
  freeNames (If    _ s1       ) = freeNames s1
  freeNames (While _ s1       ) = freeNames s1
  freeNames (Skip _           ) = Set.empty
  freeNames (Read    _  litexp) = Set.empty
  freeNames (Write   _  aexpr ) = freeNames aexpr
  freeNames (StmtSeq s1 s2    ) = freeNames s1 <> freeNames s2

instance FreeNames AExpr where
  freeNames (AInt   _               ) = Set.empty
  freeNames (ALExpr litexpr         ) = Set.singleton litexpr
  freeNames (ABinary _ aexpr1 aexpr2) = freeNames aexpr1 <> freeNames aexpr2
{-
instance FreeNames BExpr where
  usedNames (BoolConst _            ) = S.empty
  usedNames (Not       bexpr        ) = freeNames bexpr
  usedNames (BBinary _ bexpr1 bexpr2) = freeNames bexpr1 <> freeNames bexpr2
  usedNames (RBinary _ aexpr1 aexpr2) = freeNames aexpr1 <> freeNames aexpr2



instance FreeNames LiteralExpr where
  freeNames (LitVar identifier     ) = freeNames identifier
  freeNames (LitRecord identifier _) = freeNames identifier
  freeNames (LitArray  identifier _) = freeNames identifier
-}




