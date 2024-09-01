{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Main(main) where

import Control.Applicative
import Control.Monad.ST
import Data.Array.ST qualified as A
import Data.Array.Base qualified as A
import Data.ByteString qualified as BS
import Data.Functor
import Data.EnumMap.Strict qualified as EM
import Data.Vector.Unboxed.Mutable qualified as VM
import Data.Word
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

match :: (StateId q, (forall s. A.MArray (A.STUArray s) q (ST s))) => NFA q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = runST $ do
  qstack <- mkArr 24_000_000
  istack <- mkArr 24_000_000
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do A.unsafeWrite qstack s q2
                                  A.unsafeWrite istack s i
                                  go (s + 1) q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | s == 0 -> pure Failure
                | otherwise -> do q'' <- A.unsafeRead qstack (s - 1)
                                  i'' <- A.unsafeRead istack (s - 1)
                                  go (s - 1) q'' i''
  go 0 initState 0
  where
  mkArr :: A.MArray (A.STUArray s) e (ST s) => Int -> ST s (A.STUArray s Int e)
  mkArr len = A.unsafeNewArray_ (0, len)

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
