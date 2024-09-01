{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict #-}

module Main(main) where

import Control.Applicative
import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Functor
import Data.Vector qualified as V
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

type TransMap q = V.Vector (Trans q)

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
getTrans q m = m `V.unsafeIndex` fromIntegral q

match :: (VM.Unbox q, StateId q) => NFA q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = runST $ do
  stack <- VM.unsafeNew 24_000_000
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do VM.unsafeWrite stack s (q2, i)
                                  go (s + 1) q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | s == 0 -> pure Failure
                | otherwise -> do (q'', i'') <- VM.unsafeRead stack (s - 1)
                                  go (s - 1) q'' i''
  go 0 initState 0

-- * Benchmarking

nfa :: NFA Word32
nfa = NFA{..}
  where
  initState = 0
  finState = 13
  transitions = V.fromList
    [ TBranch 2 1
    , TEps 12
    , TBranch 4 8
    , TEps 0
    , TCh a 5
    , TEps 6
    , TCh a 7
    , TEps 3
    , TCh a 9
    , TEps 10
    , TCh b 11
    , TEps 3
    , TCh z 13
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
