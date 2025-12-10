module MF.Evaluating where

import           Base
import           MiniC_lib
import           MF.UsedNames
import qualified Data.Map.Strict               as Map
import           Control.Monad.Except            ( throwError )


--import           Control.Monad.Maybe


type VTable = Map.Map Name Int
type ValuesTable = Map.Map LiteralExpr Int

class (UsedNames a) => Eval a where
evalProg :: Program -> Either String ValuesTable
evalProg (Dec d) = evalDeclaration d Map.empty
evalProg (St  s) = evalStmt s Map.empty
evalProg (ProgramSeq (Dec d) (St s)) =
  evalDeclaration d Map.empty >>= evalStmt s

evalStmt :: Statement -> ValuesTable -> Either String ValuesTable
evalStmt s vs = case s of
  AssLit _ lit ae -> do
    i <- evalAE ae vs
    return $ Map.insert lit i vs
  AssRecord _ identifier aexpr1 aexpr2 -> do
    i1 <- evalAE aexpr1 vs
    i2 <- evalAE aexpr2 vs
    return
      (  Map.insert (LitRecord identifier (First 0)) i1 vs
      <> Map.insert (LitRecord identifier (Second 0)) i2 vs
      )
  IfElse bexpr s1 s2 -> do
    b <- evalBE bexpr vs
    if b then evalStmt s1 vs else evalStmt s2 vs
  If bexpr s -> do
    b <- evalBE bexpr vs
    if b then evalStmt s vs else return vs
  st@(While bexpr s1) -> do
    b <- evalBE bexpr vs
    if b then evalStmt s1 vs >>= evalStmt s else return vs
  Skip _          -> return vs
  Read    _  lit  -> return vs
  Write   _  expr -> return vs
  StmtSeq s1 s2   -> evalStmt s1 vs >>= evalStmt s2
  StEmpty         -> return vs


evalBE :: BExpr -> ValuesTable -> Either String Bool
evalBE b vs = case b of
  BoolConst _ e1 -> pure e1
  Not _ bexpr -> not <$> evalBE bexpr vs
  BBinary _ And bexpr1 bexpr2 -> (&&) <$> evalBE bexpr1 vs <*> evalBE bexpr2 vs
  BBinary _ Or bexpr1 bexpr2 -> (||) <$> evalBE bexpr1 vs <*> evalBE bexpr2 vs
  RBinary _ Less e1 e2 -> (<) <$> evalAE e1 vs <*> evalAE e2 vs
  RBinary _ LessOrEqual e1 e2 -> (<=) <$> evalAE e1 vs <*> evalAE e2 vs
  RBinary _ Greater e1 e2 -> (>) <$> evalAE e1 vs <*> evalAE e2 vs
  RBinary _ GreaterOrEqual e1 e2 -> (>=) <$> evalAE e1 vs <*> evalAE e2 vs
  RBinary _ Equality e1 e2 -> (==) <$> evalAE e1 vs <*> evalAE e2 vs
  RBinary _ Different e1 e2 -> not <$> ((==) <$> evalAE e1 vs <*> evalAE e2 vs)

evalAE :: AExpr -> ValuesTable -> Either String Int
evalAE a vs = case a of
  AInt   value           -> pure value
  ALExpr litexpr         -> evalLiteralExpr litexpr vs
  ABinary Sum      e1 e2 -> (+) <$> evalAE e1 vs <*> evalAE e2 vs
  ABinary Subtract e1 e2 -> (-) <$> evalAE e1 vs <*> evalAE e2 vs
  ABinary Multiply e1 e2 -> (*) <$> evalAE e1 vs <*> evalAE e2 vs
  ABinary Divide   e1 e2 -> div <$> evalAE e1 vs <*> evalAE e2 vs
  ABinary Modulus  e1 e2 -> mod <$> evalAE e1 vs <*> evalAE e2 vs
  --ABinary Neg e1         -> negate <$> evalAE vs e1

evalLiteralExpr :: LiteralExpr -> ValuesTable -> Either String Int
evalLiteralExpr lit vs = case lit of
  lit@(LitVar identifier) -> case Map.lookup lit vs of
    Just value -> pure value
    Nothing ->
      throwError ("eval: unbound variable '" ++ show identifier ++ "'")
  lit@(LitRecord identifier index) -> case Map.lookup lit vs of
    Just value -> pure value
    Nothing ->
      throwError ("eval: unbound variable '" ++ show identifier ++ "'")
  lit@(LitArray identifier _) -> case Map.lookup lit vs of
    Just value -> pure value
    Nothing ->
      throwError ("eval: unbound variable '" ++ show identifier ++ "'")

evalDeclaration :: Declaration -> ValuesTable -> Either String ValuesTable
evalDeclaration decl vs = case decl of
  DeclVar _ litexpr -> return $ Map.insert litexpr 0 vs
  DeclArray _ identifier (Size size) ->
    return $ Map.insert (LitArray identifier (AInt size)) 0 vs
  DeclRecord _ identifier _ _ ->
    return
      $  Map.insert (LitRecord identifier (First 0)) 0 vs
      <> Map.insert (LitRecord identifier (Second 0)) 0 vs
  DeclSeq d1 d2 -> evalDeclaration d1 vs >>= evalDeclaration d2
  DeclEmpty     -> return vs
