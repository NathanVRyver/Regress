{-# LANGUAGE OverloadedStrings #-}
module DataLoader where

import Data.Csv
import Data.Vector (Vector, toList)
import qualified Data.ByteString.Lazy as BL
import Optimizer (Sample)

data F1Record = F1Record
  { driverExperience :: Double
  , tireCompound :: Double
  , fuelLoad :: Double
  , trackTemp :: Double
  , lapTime :: Double
  } deriving (Show)

instance FromRecord F1Record where
  parseRecord v
    | length v == 5 = F1Record
        <$> v .! 0
        <*> v .! 1
        <*> v .! 2
        <*> v .! 3
        <*> v .! 4
    | otherwise = fail "Wrong number of fields"

loadF1Data :: FilePath -> IO (Either String [Sample])
loadF1Data filepath = do
  csvData <- BL.readFile filepath
  case decodeWith csvOptions HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ map recordToSample (toList records)
  where
    csvOptions = defaultDecodeOptions
    recordToSample (F1Record exp tire fuel temp time) =
      ([exp, tire, fuel, temp], time)

normalizeFeatures :: [Sample] -> [Sample]
normalizeFeatures samples = map normalizeSample samples
  where
    features = map fst samples
    means = calculateMeans features
    stds = calculateStds features means
    
    normalizeSample (xs, y) = (zipWith3 normalize xs means stds, y)
    normalize x mean std = if std == 0 then 0 else (x - mean) / std

calculateMeans :: [[Double]] -> [Double]
calculateMeans features = map mean (transpose features)
  where
    mean xs = sum xs / fromIntegral (length xs)

calculateStds :: [[Double]] -> [Double] -> [Double]
calculateStds features means = map std (zip (transpose features) means)
  where
    std (xs, mean) = sqrt $ sum [(x - mean) ^ (2 :: Int) | x <- xs] / fromIntegral (length xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose matrix = map head matrix : transpose (map tail matrix)