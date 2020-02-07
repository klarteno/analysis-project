module TestAE where

import           Main
import           PROC.MF.Analysis.AE
import           PROC.Parsing                   ( pAExprSet )

import           Data.Set                       ( Set )
import           Text.ParserCombinators.UU.Utils
                                                ( runParser )

import           Data.Foldable                  ( forM_ )
import           Data.Traversable               ( forM )
import           Text.Printf                    ( printf )

testAnalysis
  :: (Show b, Eq b)
  => Algorithm a b
  -> (Prog -> MF a)
  -> Prog
  -> [(Label, b)]
  -> IO ()
testAnalysis alg mf prog resl = forM_ resl $ \(l, expected) -> do
  let fnd = analyse alg mf prog l
  if expected == fnd
    then return ()
    else fail
      (printf "expected %s, found %s (at %d)" (show expected) (show fnd) l)

mainAE :: IO ()
mainAE = do
  testAnalysis mfp      mfAE progAE reslAE
  testAnalysis (mopk 3) mfAE progAE reslAE

progAE :: Prog
progAE = mkProg
  [ "x = a + b;"
  , "y = a * b;"
  , "while (y > a + b) {"
  , "  a = a + 1;"
  , "  x = a + b;"
  , "}"
  ]

reslAE :: [(Label, Set AExpr)]
reslAE = aexprs ["{a + b}", "{a * b, a + b}", "{a + b}", "{}", "{a + b}"]

aexprs :: [String] -> [(Label, Set AExpr)]
aexprs = zip [1 ..] . map (runParser "stdin" pAExprSet)
