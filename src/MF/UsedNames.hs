module MF.UsedNames where

    import           MiniC_lib
    import           Data.Monoid                    ( (<>) )
    import qualified Data.Set                      as Set
    
    -- * Left Assigned Variable Names
    
    class UsedNames a where
      usedNames :: a -> Set.Set IdentifierName
      isUsedIn :: IdentifierName -> a -> Bool
      isUsedIn name a = Set.member name (usedNames a)
    
    instance UsedNames Program where
      usedNames (Dec d) = Set.empty
      usedNames (St s) = usedNames s
      usedNames (ProgramSeq p1 p2) = usedNames p1 <> usedNames p2
       -- foldr ((<>) . usedNames) S.empty xs --foldl (<>) S.empty (map usedNames xs)

    instance UsedNames Statement where
      usedNames (AssLit _ litexp _ ) = usedNames litexp
      usedNames (AssRecord _ identifier aexpr1 aexpr2) = usedNames identifier
      usedNames (IfElse bexpr s1 s2) = usedNames bexpr <> usedNames s1 <> usedNames s2
      usedNames (If bexpr s1) = usedNames bexpr <> usedNames s1
      usedNames (While bexpr s1) = usedNames bexpr <> usedNames s1
      usedNames (Skip _) = Set.empty
      usedNames (Read _ litexp) = usedNames litexp
      usedNames (Write _ aexpr) = usedNames aexpr 

    instance UsedNames LiteralExpr where
      usedNames (LitVar identifier) = usedNames identifier     
      usedNames (LitRecord identifier _ ) = usedNames identifier     
      usedNames (LitArray identifier _ ) = usedNames identifier     

    instance UsedNames AExpr where
      usedNames (AInt _) = Set.empty         
      usedNames (ALExpr litexp) = usedNames litexp         
      usedNames (ABinary _ aexpr1 aexpr2  ) = usedNames aexpr1 <> usedNames aexpr2  
         
    instance UsedNames BExpr where
      usedNames (BoolConst _ _) = Set.empty        
      usedNames (Not _ bexpr) = usedNames bexpr        
      usedNames (BBinary _ _ bexpr1 bexpr2) = usedNames bexpr1 <> usedNames bexpr2       
      usedNames (RBinary _ _ aexpr1 aexpr2) = usedNames aexpr1 <> usedNames aexpr2           

    instance UsedNames IdentifierName where
      usedNames (IdentifierName name ) = Set.singleton (IdentifierName name)         
    


    --- let ident = IdentifierName "fff"
    --- let answ = isUsedIn ident (LitVar (IdentifierName "fff"))
