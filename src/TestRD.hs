module TestRD where

import           Main
import           MF.Analysis.RD
import           MF.Analysis
import           MF.Labelable
import           MF.Algorithm.MFP
import           MiniC_lib
import           Data.Set                       ( Set )
import           Text.ParserCombinators.UU.Utils
                                                ( runParser )

import           Data.Foldable                  ( forM_ )
import           Text.Printf                    ( printf )

testAnalysis
  :: (Show b, Eq b)
  => Algorithm a b
  -> (Program -> MF a)
  -> Program
  -> [(Label, b)]
  -> IO ()
testAnalysis alg mf prog resl = forM_ resl $ \(l, exp) -> do
  let fnd = analyse alg mf prog l
  if exp == fnd
    then return ()
    else fail (printf "expected %s, found %s (at %d)" (show exp) (show fnd) l)

mainRD :: IO ()
mainRD = do
  testAnalysis mfp      mfRD progRD reslRD
  testAnalysis (mopk 3) mfRD progRD reslRD

progRD :: Program
progRD = mkProg
  ["x = 5;", "y = 1;", "while (x > 1) {", "  y = x * y;", "  x = x - 1;", "}"]

reslRD :: [(Label, Set RD)]
reslRD = rds
  [ "{(y,?), (x,1)}"
  , "{(x,1), (y,2)}"
  , "{(x,1), (y,2), (y,4), (x,5)}"
  , "{(x,1), (y,4), (x,5)}"
  , "{(x,1), (y,4)}"
  ]

rds :: [String] -> [(Label, Set RD)]
rds = zip [1 ..] . map (runParser "stdin" pRDSet)
