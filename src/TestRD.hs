module TestRD where

import           MF.Analysis
import           MiniC_lib

import           Data.Foldable                  ( forM_ )
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
    else fail (printf "expected %s, found %s (at %d)" (show expected) (show fnd) l)

-- TODO: Re-implement test functions with proper parsing
-- mainRD :: IO ()
-- mainRD = do
--   testAnalysis mfp      mfRD progRD reslRD
--   testAnalysis (mopk 3) mfRD progRD reslRD
--
-- progRD :: Program
-- progRD = mkProg
--   ["x = 5;", "y = 1;", "while (x > 1) {", "  y = x * y;", "  x = x - 1;", "}"]
--
-- reslRD :: [(Label, Set RD)]
-- reslRD = rds
--   [ "{(y,?), (x,1)}"
--   , "{(x,1), (y,2)}"
--   , "{(x,1), (y,2), (y,4), (x,5)}"
--   , "{(x,1), (y,4), (x,5)}"
--   , "{(x,1), (y,4)}"
--   ]
--
-- rds :: [String] -> [(Label, Set RD)]
-- rds = zip [1 ..] . map (runParser "stdin" pRDSet)
