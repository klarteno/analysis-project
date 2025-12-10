module TestAE where

import           Base
import           MF.Analysis
import           MF.Analysis.AE
import           MiniC_lib

import           Data.Set                       ( Set )

import           Data.Foldable                  ( forM_ )
import           Data.Traversable               ( forM )
import           Text.Printf                    ( printf )

testAnalysis
  :: (Show b, Eq b)
  => Algorithm a b
  -> (Program -> MF a)
  -> Program
  -> [(Label, b)]
  -> IO ()
testAnalysis alg mf prog resl = forM_ resl $ \(l, expected) -> do
  let fnd = analyse alg mf prog l
  if expected == fnd
    then return ()
    else fail
      (printf "expected %s, found %s (at %d)" (show expected) (show fnd) l)

-- TODO: Re-implement test functions with proper parsing
-- mainAE :: IO ()
-- mainAE = do
--   testAnalysis mfp      mfAE progAE reslAE
--   testAnalysis (mopk 3) mfAE progAE reslAE
--
-- progAE :: Prog
-- progAE = mkProg
--   [ "x = a + b;"
--   , "y = a * b;"
--   , "while (y > a + b) {"
--   , "  a = a + 1;"
--   , "  x = a + b;"
--   , "}"
--   ]
--
-- reslAE :: [(Label, Set AExpr)]
-- reslAE = aexprs ["{a + b}", "{a * b, a + b}", "{a + b}", "{}", "{a + b}"]
--
-- aexprs :: [String] -> [(Label, Set AExpr)]
-- aexprs = zip [1 ..] . map (runParser "stdin" pAExprSet)
