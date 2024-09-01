{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main(main) where

import Control.Applicative
import Data.ByteString qualified as BS
import Data.Functor
import Data.EnumMap.Strict qualified as EM
import Data.Vector.Mutable.Linear qualified as VL
import Data.Unrestricted.Linear qualified as L
import Data.Word
import Prelude.Linear qualified as L
import System.Environment
import System.IO.MMap

-- * Definitions

type StateId q = (Integral q, Enum q)

data Trans q
  = TEps q
  | TBranch q q
  | TCh Word8 q
  deriving (Eq, Show)

type TransMap q = EM.EnumMap q (Trans q)

data NFA q = NFA
  { transitions :: TransMap q
  , initState :: q
  , finState :: q
  }

data MatchResult a = SuccessAt a | Failure
  deriving (Eq, Ord, Show, Functor)

instance Applicative MatchResult where
  SuccessAt f <*> SuccessAt v = SuccessAt $ f v
  _           <*> _           = Failure
  pure = SuccessAt

instance Alternative MatchResult where
  SuccessAt a <|> ~_ = SuccessAt a
  _           <|> ~r = r
  empty = Failure


-- * Matching

getTrans :: StateId q => q -> TransMap q -> Trans q
getTrans q m = case q `EM.lookup` m of
                 Just t -> t
                 Nothing -> error "invariant failure"

match :: forall q. StateId q => NFA q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = L.unur L.$ VL.empty L.$ go initState 0
  where
  go :: q -> Int -> VL.Vector (q, Int) %1-> L.Ur (MatchResult Int)
  go q i stack
    | q == finState = stack `L.lseq` L.Ur (SuccessAt i)
    | otherwise = case q `getTrans` transitions of
                    TEps q' -> go q' i stack
                    TBranch q1 q2 -> go q1 i L.$ (q2, i) `VL.push` stack
                    TCh ch q'
                      | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1) stack
                      | otherwise -> case VL.pop stack of
                                      (L.Ur top, stack'')
                                        | (Just (q'', i'')) <- top -> go q'' i'' stack''
                                        | otherwise -> stack'' `L.lseq` L.Ur Failure


-- * Benchmarking

nfa :: NFA Word32
nfa = NFA{..}
  where
  initState = 0
  finState = 13
  transitions = EM.fromList
    [ (0, TBranch 2 1)
    , (1, TEps 12)
    , (2, TBranch 4 8)
    , (3, TEps 0)
    , (4, TCh a 5)
    , (5, TEps 6)
    , (6, TCh a 7)
    , (7, TEps 3)
    , (8, TCh a 9)
    , (9, TEps 10)
    , (10, TCh b 11)
    , (11, TEps 3)
    , (12, TCh z 13)
    ]
  a = 97
  b = 98
  z = 122

main :: IO ()
main = do
  path <- getArgs <&> \case [path] -> path
                            _ -> error "wrong usage"
  str <- mmapFileByteString path Nothing
  print $ match nfa str
