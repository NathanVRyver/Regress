{-# LANGUAGE OverloadedStrings #-}
module Regress.Data
  ( loadCSV
  , normalizeFeatures
  ) where

import Data.Csv
import Data.Vector (Vector, toList)
import qualified Data.ByteString.Lazy as BL
import Regress.Core (Sample)

loadCSV :: FilePath -> IO (Either String [Sample])
loadCSV filepath = do
  csvData <- BL.readFile filepath
  case decodeWith csvOptions HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ map Regress.Data.parseRecord (toList records)
  where
    csvOptions = defaultDecodeOptions

parseRecord :: [Double] -> Sample
parseRecord xs = (init xs, last xs)

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