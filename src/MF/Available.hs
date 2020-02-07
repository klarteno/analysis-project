module MF.Available where

import           MiniC_lib
import           MF.Flowable
-- import           Base
import           Data.Monoid                    ( (<>) )
import qualified Data.Set                      as Set
import qualified Data.Foldable                 as F
                                                ( foldMap )

-- * Available Expressions
getavailable :: Program -> Set.Set AExpr
getavailable = F.foldMap available . blocks

class Available a where
  --scanBlock :: a -> Set.Set AExpr
  ---scanBlock a = Set.empty
  available :: a -> Set.Set AExpr

instance Available Block where
  available (BlockSt st) = available st
  available (BlockB  b ) = available b
  available (BlockE  d ) = Set.empty

instance Available Statement where
  available (AssLit _ _ aexpr) = available aexpr
  available (AssRecord _ _ aexpr1 aexpr2) =
    available aexpr1 <> available aexpr2
  available (Read  _ _    ) = Set.empty
  available (Write _ aexpr) = available aexpr

instance Available AExpr where
  available (AInt   _     ) = Set.empty
  available (ALExpr litexp) = Set.empty
  available av@(ABinary op aexpr1 aexpr2) =
    Set.insert av (available aexpr1 <> available aexpr2)

instance Available BExpr where
  available (BoolConst _ _            ) = Set.empty
  available (Not       _ bexpr        ) = available bexpr
  available (BBinary _ _ bexpr1 bexpr2) = available bexpr1 <> available bexpr2
  available (RBinary _ _ aexpr1 aexpr2) = available aexpr1 <> available aexpr2


  {- 
class Available a where
  available :: a -> Set.Set AExpr

instance (Available a) => (Available [a]) where
  available = foldr ((<>) . available) Set.empty

instance Available Program where
  available (Dec d           ) = Set.empty
  available (St  s           ) = available s
  available (ProgramSeq p1 p2) = available p1 <> available p2
    -- foldr ((<>) . available) S.empty xs
instance Available Statement where
  available = F.foldMap (available' . blocks2) . blocks
   where
    blocks2 (BlockSt st) = st

    available' (AssLit _ _ aexpr) = available aexpr
    available' (AssRecord _ _ aexpr1 aexpr2) =
      available aexpr1 <> available aexpr2
    available' (IfElse bexpr s1 s2) =
      available bexpr <> available s1 <> available s2
    available' (If    bexpr s1) = available bexpr <> available s1
    available' (While bexpr s1) = available bexpr <> available s1
    available' (Skip _        ) = Set.empty
    available' (Read  _ litexp) = available litexp
    available' (Write _ aexpr ) = available aexpr

instance Available LiteralExpr where
  available (LitVar identifier     ) = Set.empty
  available (LitRecord identifier _) = Set.empty
  available (LitArray  identifier _) = Set.empty

instance Available AExpr where
  available (AInt   _     ) = Set.empty
  available (ALExpr litexp) = Set.empty
  available av@(ABinary op aexpr1 aexpr2) =
    Set.insert av (available aexpr1 <> available aexpr2)

instance Available BExpr where
  available (BoolConst _ _            ) = Set.empty
  available (Not       _ bexpr        ) = available bexpr
  available (BBinary _ _ bexpr1 bexpr2) = available bexpr1 <> available bexpr2
  available (RBinary _ _ aexpr1 aexpr2) = available aexpr1 <> available aexpr2

-}
