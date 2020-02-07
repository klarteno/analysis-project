
module MF.Flowable where

import           Prelude                 hiding ( init )
import           Base
import           MiniC_lib
import           Text.Printf                    ( printf )
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import qualified Data.Foldable                 as S
                                                ( foldMap )
import qualified Data.Traversable              as S
                                                ( forM )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Control.Applicative            ( pure
                                                , (<$>)
                                                , (<*>)
                                                )

import           Data.Maybe                     ( catMaybes )
import           Data.Monoid



data Flow
  = Intra Label Label
  deriving (Eq,Ord)

type IntraFlow = (Label, Label)

instance Show Flow where
  show (Intra x y) = printf "(%s,%s)" (show x) (show y)

-- |Reverses a @Flow@ tuple.
swap :: Flow -> Flow
swap (Intra a b) = Intra b a

-- |Converts @Flow@ instances to values.
getVals :: Flow -> Maybe IntraFlow
getVals (Intra a b) = Just (a, b)
getVals _           = Nothing

-- |Returns whether or not a flow is valid.
isIntra :: Flow -> Bool
isIntra (Intra _ _) = True
isIntra _           = False

-- |Uses the declarations in a @Program@  to extract flows
progFlow, progFlowR :: Program -> Set Flow
progFlow p@(Dec d)            = flow (mkFTable d) p
progFlow _                    = Set.empty
progFlow p@(ProgramSeq p1 p2) = undefined
progFlowR p@(Dec d) = flow (mkFTable d) p
progFlowR _         = Set.empty

-- |Determines whether a flow begins in a specific label.
flowsFrom, flowsTo :: Flow -> Label -> Bool
flowsFrom (Intra first _) label = first == label
-- |Determines whether a flow ends in a specific label.
flowsTo (Intra _ second) label = second == label

-- |Act as @fst@ and @snd@ on @Flow@ tuples, respectively.
from, to :: Flow -> Label
from (Intra l _) = l
to (Intra _ l) = l

data Block = BlockSt Statement
            | BlockB BExpr
            | BlockE Declaration
              deriving (Eq,Ord,Show)

class Flowable a where
  init   :: a -> Label
  final  :: a -> Set Label
  labels  :: a -> Set Label
  blocks :: a -> Set Block
 -- labels = S.foldMap  fmap (fmap labels) blocks
  flow,flowR   :: FTable -> a -> Set Flow
  flowR e = Set.map swap . flow e
  entry  :: FTable -> a -> Label
  entry e = takeOne . Set.elems . Set.map to . entry'
    where
    entry'    a  = Set.map (\l -> Intra l (init a)) (labels a) \\ flow e a
    takeOne [ ] = error "exit"
    takeOne [x] = x
    takeOne  _  = error "exit"
  exits  :: FTable -> a -> Set Label
  exits e = Set.map from . exits'
    where
    exits' a = S.foldMap (\l1 -> Set.map (Intra l1) (labels a)) (final a) \\ flow e a

instance Flowable Program where
  init (Dec decl        ) = Label Nothing
  init (St  s           ) = init s
  init (ProgramSeq p1 p2) = case (p1, p2) of
    (St stm1, St stm2) -> init stm1
    (_      , St stm2) -> init stm2
--S.foldMap init xs
  final (Dec decl        ) = Set.empty
  final (St  s           ) = final s
  final (ProgramSeq p1 p2) = final p2

  blocks (Dec decl        ) = blocks decl
  blocks (St  s           ) = blocks s
  blocks (ProgramSeq p1 p2) = blocks p1 <> blocks p2

  labels (Dec decl        ) = labels decl
  labels (St  s           ) = labels s
  labels (ProgramSeq p1 p2) = labels p1 <> labels p2

  flow e (Dec decl        ) = flow e decl
  flow e (St  s           ) = flow e s
  flow e (ProgramSeq p1 p2) = flow e p1 <> flow e p2

instance Flowable Declaration where
  init (DeclSeq d1 d2) = init d1
  init _               = Label Nothing

  final (DeclSeq d1 d2) = final d2
  final _               = Set.empty

  blocks b@DeclVar{}     = Set.singleton (BlockE b)
  blocks b@DeclArray{}   = Set.singleton (BlockE b)
  blocks b@DeclRecord{}  = Set.singleton (BlockE b)
  blocks (DeclSeq d1 d2) = blocks d1 <> blocks d2
  blocks b@DeclEmpty     = Set.singleton (BlockE b)

  labels (DeclSeq d1 d2) = labels d1 <> labels d2
  labels _               = Set.empty

  flow e (DeclSeq d1 d2) = flow e d1 <> flow e d2
  flow _ _               = Set.empty
-- some123 = foldl (+) 0 [1, 2, 3]

instance Flowable Statement where
  init (AssLit label litexpr _) = label
  init (AssRecord label _ _ _ ) = label
  init (IfElse b _ _          ) = init b
  init (If    b _             ) = init b
  init (While b _             ) = init b
  init (Skip label            ) = label
  init (Read    label _       ) = label
  init (Write   label _       ) = label
  init (StmtSeq s1    s2      ) = init s1

  final (AssLit label litexpr _) = Set.singleton label
  final (AssRecord label _ _ _ ) = Set.singleton label
  final (IfElse b s1 s2        ) = final s1 <> final s2
  final (If    b s1            ) = final s1
  final (While b _             ) = final b --recursion statements and that why condition is final
  final (Skip label            ) = Set.singleton label
  final (Read    label _       ) = Set.singleton label
  final (Write   label _       ) = Set.singleton label
  final (StmtSeq s1    s2      ) = final s2

  labels (AssLit label _ _     ) = Set.singleton label
  labels (AssRecord label _ _ _) = Set.singleton label
  labels (IfElse b s1 s2       ) = labels b <> labels s1 <> labels s2
  labels (If    b s1           ) = labels b <> labels s1
  labels (While b s            ) = labels b <> labels s
  labels (Skip label           ) = Set.singleton label
  labels (Read    label _      ) = Set.singleton label
  labels (Write   label _      ) = Set.singleton label
  labels (StmtSeq s1    s2     ) = labels s1 <> labels s2

  blocks stm@AssLit{}             = Set.singleton (BlockSt stm)
  blocks stm@AssRecord{}          = Set.singleton (BlockSt stm)
  blocks stm@(IfElse b s1 s2)     = blocks b <> blocks s1 <> blocks s2
  blocks stm@(If    b s1    )     = blocks b <> blocks s1
  blocks stm@(While b s     )     = blocks b <> blocks s
  blocks stm@Skip{}               = Set.singleton (BlockSt stm)
  blocks stm@(Read    _  litexpr) = Set.singleton (BlockSt stm)
  blocks stm@(Write   _  aexpr  ) = Set.singleton (BlockSt stm)
  blocks stm@(StmtSeq s1 s2     ) = blocks s1 <> blocks s2

  flow e AssLit{}         = Set.empty
  flow e AssRecord{}      = Set.empty
  flow e (IfElse b s1 s2) = bool_to_inits <> flow e s1 <> flow e s2
   where
    bool_to_inits = Set.map (Intra (init b) . init) (Set.fromList [s1, s2])
  flow e (If    b s1) = Set.singleton (Intra (init b) (init s1)) <> flow e s1
  flow e (While b s1) = bool_to_init <> flow e s1 <> finals_to_bool
   where
    bool_to_init   = Set.singleton (Intra (init b) (init s1))
    finals_to_bool = Set.map (\l -> Intra l (init b)) (final s1)
  flow e (Skip _            ) = Set.empty
  flow e (Read    _  litexpr) = Set.empty
  flow e (Write   _  aexpr  ) = Set.empty
  flow e (StmtSeq s1 s2     ) = flow e s1 <> flow e s2 <> finals_to_init
    where finals_to_init = Set.map (\l -> Intra l (init s2)) (final s1)

instance Flowable BExpr where
  init (BoolConst label _  ) = label
  init (Not       label _  ) = label
  init (BBinary label _ _ _) = label
  init (RBinary label _ _ _) = label

  final (BoolConst label _  ) = Set.singleton label
  final (Not       label _  ) = Set.singleton label
  final (BBinary label _ _ _) = Set.singleton label
  final (RBinary label _ _ _) = Set.singleton label

  labels (BoolConst label _  ) = Set.singleton label
  labels (Not       label _  ) = Set.singleton label
  labels (BBinary label _ _ _) = Set.singleton label
  labels (RBinary label _ _ _) = Set.singleton label

  blocks b@(BoolConst _ _) = Set.singleton (BlockB b)
  blocks b@(Not       _ _) = Set.singleton (BlockB b)
  blocks b@BBinary{}       = Set.singleton (BlockB b)
  blocks b@RBinary{}       = Set.singleton (BlockB b)

  flow e _ = Set.empty

