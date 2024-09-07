{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module ProcessData(process) where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.List (groupBy, isSuffixOf, genericLength, sortBy, minimumBy)
import Data.Map qualified as M
import Data.Ord
import Data.String.Interpolate
import System.Directory

data Measurement = Measurement
  { mutTime :: Double
  , gcTime :: Double
  , totalTime :: Double
  } deriving (Show)

newtype TestFileName = TestFileName { getName :: String } deriving newtype (Show, Eq, Ord)

data TestFileInfo a = TestFileInfo
  { testFile :: TestFileName
  , testInfo :: a
  } deriving (Show, Functor)

type MeasSet = TestFileInfo [Measurement]

parseTime :: String -> Either String Double
parseTime s
  | (_:_:timeStr:_) <- words s = pure $ read $ init timeStr
  | otherwise = Left $ "invalid format: " <> s

parseMeas :: String -> String -> String -> Either String Measurement
parseMeas mutStr gcStr totalStr = Measurement <$> parseTime mutStr <*> parseTime gcStr <*> parseTime totalStr

collectMeasSet :: [String] -> Either String MeasSet
collectMeasSet [] = Left "empty group"
collectMeasSet (x:xs) = TestFileInfo (TestFileName x) <$> go xs
  where
  go (mut : gc : total : rest) = (:) <$> parseMeas mut gc total <*> go rest
  go [] = pure []
  go strs = Left $ "wrong measurements: " <> show strs

newtype SourceVersion = SourceVersion { getSV :: String } deriving newtype (Show, Semigroup, Monoid, Eq, Ord)

parseContents :: String -> Either String (SourceVersion, [MeasSet])
parseContents file = case lines file of
                       [] -> Left "empty file"
                       (header : rest) -> (SourceVersion $ drop 2 header, ) <$> mapM collectMeasSet (groupBy testSecGrp rest)
  where
  testSecGrp _ (' ':_) = True
  testSecGrp _ _ = False

data Stats = Stats
  { minMutTime :: Double
  , minGcTime :: Double
  , totalTimeDev :: Double
  } deriving (Show)

calcStats :: [Measurement] -> Stats
calcStats ms = Stats{..}
  where
  Measurement{totalTime = minTotalTime, mutTime = minMutTime, gcTime = minGcTime} = minimumBy (comparing totalTime) ms
  totalTimeDev = sqrt $ sum [ (totalTime m - minTotalTime) ** 2 | m <- ms ] / (genericLength ms - 1)

regroup :: [(SourceVersion, [TestFileInfo Stats])] -> [(TestFileName, [(SourceVersion, Stats)])]
regroup datas = map (second $ map (first $ \(SourceVersion v) -> SourceVersion (drop 3 v))
                            . sortBy (comparing fst))
              $ M.toList
              $ M.fromListWith (<>)
  [ (testFile, [(sv, testInfo)])
  | (sv, tests) <- datas
  , TestFileInfo{..} <- tests
  ]

process :: IO ()
process = do
  files <- getDirectoryContents "data"
  parsed <- forM (filter (".txt" `isSuffixOf`) files) $ \file -> do
    meas <- readFile ("data/" <> file) <&> parseContents
    case meas of
      Left err -> error err
      Right (n, res) -> pure (SourceVersion (take 2 file <> " ") <> n, fmap calcStats <$> res)

  writeFile "data.gnuplot" [i|File  Version       "MUT time" "Deviation" "GC time"\n|]
  forM_ (regroup parsed) $ \(testFile, stats) ->
    forM_ stats $ \(sv, Stats{..}) -> do
      appendFile "data.gnuplot" [i|#{name testFile}\t\t"#{sv}"\t\t#{minMutTime}\t#{totalTimeDev}\t#{minGcTime}\n|]
  where
  name file = reverse $ drop 4 $ reverse $ drop 9 $ getName file
