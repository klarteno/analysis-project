module TestAst where

import           Prelude
import           Base

import           MF.Labelable
import           MF.Evaluating
import           MiniC_lib
import qualified Data.Map.Strict               as Map
import           Data.Either

main = putStr "gsdgsdg"

mkProg :: String -> Program
mkProg = parseProgram
--mkProg = parseProgram . unlines

runProg :: Program -> Maybe (LiteralExpr, Int)
runProg p = either (const Nothing) (Just . Map.elemAt 0) (evalProg p)



ast1 = mkProg "y:= x; if (true && false) {y:= x }"
ast2 = mkProg "y:= x; if (3<4) {y:= x ;} else { z:= z*2;};if (3<4) {n:= n*9;}"
ast3 = mkProg "x := 0;while (3 < 5) { x := x + 1;}"

ast4 = mkProg
  "x := 2; y := 4; z := 1; if (4 > 2) {z := y;} else {z := y * y;}; x := z;"

ast5 = mkProg "x := 0; z := 1; while (x<2) {z:= z*2; x:= x+1;}"

ast6 = mkProg "x := 0; while (x < 12) {  x := x + 1;}"

result1 = evalProg ast6
result2 = evalProg ast5
