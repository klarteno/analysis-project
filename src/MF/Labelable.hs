module MF.Labelable where

import           MiniC_lib
import           MF.Supply                      ( Supply
                                                , supply
                                                , evalSupply
                                                )

class Labelable a where
  label :: a -> Supply Int a
  runLabel :: a -> a
  runLabel a = evalSupply (label a) [1..]

instance (Labelable a) => Labelable [a] where
  label = mapM label

instance Labelable Program where
  label (Dec d) = return (Dec d)
  label (St  s) = do
    s <- label s
    return (St s)
  label (ProgramSeq p1 p2) = do
    p1 <- label p1
    p2 <- label p2
    return (ProgramSeq p1 p2)

instance Labelable Statement where
  label (AssLit _ litexpr aexpr) = do
    l       <- supply
    litexpr <- label litexpr
    aexpr   <- label aexpr
    return (AssLit (Label (Just l)) litexpr aexpr)
  label (AssRecord _ ident aexpr1 aexpr2) = do
    l      <- supply
    aexpr1 <- label aexpr1
    aexpr2 <- label aexpr2
    return (AssRecord (Label (Just l)) ident aexpr1 aexpr2)
  label (IfElse b s1 s2) = do
    b  <- label b
    s1 <- label s1
    s2 <- label s2
    return (IfElse b s1 s2)
  label (If b s1) = do
    b  <- label b
    s1 <- label s1
    return (If b s1)
  label (While b s) = do
    b  <- label b
    s1 <- label s
    return (While b s)
  label (Skip _) = do
    l <- supply
    return (Skip (Label (Just l)))
  label (Read _ litexpr) = do
    l       <- supply
    litexpr <- label litexpr
    return (Read (Label (Just l)) litexpr)
  label (Write _ aexpr) = do
    l     <- supply
    aexpr <- label aexpr
    return (Write (Label (Just l)) aexpr)
  label (StmtSeq s1 s2) = do
    s1 <- label s1
    s2 <- label s2
    return (StmtSeq s1 s2)
  label StEmpty = return StEmpty

instance Labelable BExpr where
  label (BoolConst _ val) = do
    l <- supply
    return (BoolConst (Label (Just l)) val)
  label (Not _ val) = do
    l <- supply
    return (Not (Label (Just l)) val)
  label (BBinary _ val1 val2 val3) = do
    l <- supply
    return (BBinary (Label (Just l)) val1 val2 val3)
  label (RBinary _ val1 val2 val3) = do
    l <- supply
    return (RBinary (Label (Just l)) val1 val2 val3)

instance Labelable LiteralExpr where
  label (LitVar val         ) = return (LitVar val)
  label (LitRecord val1 val2) = return (LitRecord val1 val2)
  label (LitArray  val1 val2) = return (LitArray val1 val2)

instance Labelable AExpr where
  label (AInt   val    ) = return (AInt val)
  label (ALExpr litexpr) = do
    litexpr <- label litexpr
    return (ALExpr litexpr)
  label (ABinary op aexpr1 aexpr2) = do
    aexpr1 <- label aexpr1
    aexpr2 <- label aexpr2
    return (ABinary op aexpr1 aexpr2)


parseProgram :: String -> Program
parseProgram = runLabel . parseProg

-- parseProgram "y:= x; if (true && false) {y:= x }"
 -- parseProgram "y:= x; if (3<4) {y:= x ;} else { z:= z*2;};if (3<4) {n:= n*9;}"

