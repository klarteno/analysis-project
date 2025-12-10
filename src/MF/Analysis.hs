{-# LANGUAGE ExistentialQuantification #-}
module MF.Analysis where

import           Prelude                 hiding ( init )
import           MiniC_lib
import           Base
import           MF.Flowable


import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as S

import           Text.Printf                    ( printf )



-- |Runs an Analysis analysis on a program and prints the result to stdout.
analyseAndPrint
  :: (Show b) => Algorithm a b -> (Program -> MF a) -> Program -> IO ()
analyseAndPrint = ((putStrLn .) .) . analyseAndShow

-- |Runs an Analysis analysis on a program and shows the result.
analyseAndShow
  :: (Show b) => Algorithm a b -> (Program -> MF a) -> Program -> String
analyseAndShow alg mf p = showAnalysis (S.toList $ labels p) (analyse alg mf p)

-- |Show instances of Analysis--which are functions--for a limited
--  number of inputs.
showAnalysis :: (Show a) => [Label] -> Analysis a -> String
showAnalysis ls analysis =
  unlines $ map (\l -> printf "%d: %s" l (show $ analysis l)) ls

-- |An analysis is a function that contains information for all valid labels.
type Analysis a = Label -> a

-- |A method is an algorithm for obtaining an analysis from an MF and a program.
type Algorithm a b = MF a -> Analysis b

-- |Runs an Analysis analysis on a program at a certain label.
analyse :: Algorithm a b -> (Program -> MF a) -> Program -> Analysis b
analyse alg mkMF dec@(Dec d) label
  | S.member label (labels dec) = alg (mkMF dec) label
  | otherwise = error ("no statement with label " ++ show label)
analyse alg mkMF st@(St s) label
  | S.member label (labels st) = alg (mkMF st) label
  | otherwise = error ("no statement with label " ++ show label)
analyse alg mkMF p@(ProgramSeq p1 p2) label
  | S.member label (labels p) = alg (mkMF p) label
  | otherwise                 = error ("no statement with label " ++ show label)
-- * Monotone Frameworks

-- |Type for transfer functions in an MF.
type Transfer a = Statement -> a -> a

data Direction = Forwards | Backwards

isForwards :: MF a -> Bool
isForwards mf = case getDirection mf of
  Forwards -> True
  _        -> False

isBackwards :: MF a -> Bool
isBackwards mf = case getDirection mf of
  Backwards -> True
  _         -> False

data MF a = MF
  { getI         :: a                          -- ^ extremal values
  , getE         :: Set Label                  -- ^ extremal labels
  , getF         :: Set Flow                   -- ^ control flow for analysis
  , getL         :: Lattice a                  -- ^ lattice on property space
  , getT         :: Transfer a                 -- ^ transfer function
  , getD         :: FTable                     -- ^   declarations
  , getDirection :: Direction                  -- ^ direction of the analysis
  , getBlocks    :: Set Block                   -- ^ blocks in a program
  }

-- * Lattices

data Lattice a = Lattice
  { join    :: a -> a -> a
  , refines :: a -> a -> Bool
  , bottom  :: a
  }

-- |Joins a list of values.
joinall :: Lattice a -> [a] -> a
joinall l = foldr (join l) (bottom l)

-- | make MF's for backwards analyses.
forwards :: Program -> MF a -> MF a
forwards decl@(Dec d) mf = mf { getE         = S.singleton (init d)
                              , getF         = progFlow decl
                              , getDirection = Forwards
                              }
forwards st@(St s) mf = mf { getE         = S.singleton (init s)
                           , getF         = progFlow st
                           , getDirection = Forwards
                           }
forwards p@(ProgramSeq p1 p2) mf = mf
  { getE         = S.singleton (init p1) <> S.singleton (init p2)
  , getF         = progFlow p
  , getDirection = Forwards
  }

-- | make MF's for forwards analyses.
backwards :: Program -> MF a -> MF a
backwards decl@(Dec d) mf =
  mf { getE = final d, getF = progFlowR decl, getDirection = Backwards }
backwards st@(St s) mf =
  mf { getE = final s, getF = progFlowR st, getDirection = Backwards }
backwards p@(ProgramSeq p1 p2) mf =
  mf { getE = final p2, getF = progFlowR p
                                --, getIF        = progInterFlowR p
                                          , getDirection = Backwards }

-- |Type for @kill@ functions of distributive MF's.
type Kill a = Statement -> a -> a

-- |Type for @gen@ functions of distributive MF's.
type Gen a = Statement -> a

-- |Easily make distributive monotone frameworks.
distributive
  :: (Ord a) => Kill (Set a) -> Gen (Set a) -> MF (Set a) -> MF (Set a)
distributive kill gen mf = mf { getT = transfer }
 where
  transfer s rs = (rs \\ killed) <> gen s
    where killed = kill s (bottom $ getL mf)

-- |Provides the instance with a table of declarations,
--  and   the blocks in the program.
getElements :: Program -> MF a -> MF a
getElements decl@(Dec d) mf = mf { getD = mkFTable d, getBlocks = blocks decl }
getElements st@(  St  s) mf = mf { getBlocks = blocks st }
getElements p@(ProgramSeq (Dec d) s) mf =
  mf { getD = mkFTable d, getBlocks = blocks p }

-- |Empty monotone framework.
framework :: MF a
framework = MF
  { getI         = error "uninitialized property 'I'"
  , getE = error "uninitialized property 'E' (apply 'backwards' or 'forwards')"
  , getF = error "uninitialized property 'F' (apply 'backwards' or 'forwards')"
  --, getIF        = error "uninitialized property 'IF' (apply 'backwards' or 'forwards')"
  , getL         = error "uninitialized property 'L'"
  , getT         = error "uninitialized property 'T'"
  , getD         = error "uninitialized property 'D' (apply 'embelished')"
  , getDirection = error "uninitialized property 'direction'"
  , getBlocks    = error "uninitialized property 'blocks' (apply 'getElements')"
  }

  -- |Represents the information returned by Reached-Definition Analysis.
--  It stores whether or not a variable has been assigned to, and if,
--  in what label. For instance, @{x,?}@ means that the variable @x@ has
--  not yet been assigned to, whereas @{y,5}@ has (at this point in the
--  program) been assigned a value in label @5@.
data RD = RD LiteralExpr Label

instance Show RD where
  show (RD x (Label Nothing      )) = printf "{%s,?}" x
  show (RD x (Label (Just number))) = printf "{%s,%d}" x number

instance Eq RD where
  (RD x _) == (RD y _) = x == y

instance Ord RD where
  compare (RD x _) (RD y _) = compare x y

