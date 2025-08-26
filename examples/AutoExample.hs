{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (toList)
import Regress

data AutoRow = AutoRow Double Double Double Double Double Double Double Double

instance FromRecord AutoRow where
  parseRecord v = AutoRow <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 
                          <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7

loadAutoData :: FilePath -> IO (Either String [Sample])
loadAutoData filepath = do
  csvData <- BL.readFile filepath
  case decodeWith csvOptions HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ concatMap toSample (toList records)
  where
    csvOptions = defaultDecodeOptions
    toSample (AutoRow mpg cylinders displacement horsepower weight acceleration year origin)
      | horsepower == 0 = []
      | otherwise = [([cylinders, displacement, horsepower, weight, acceleration, year, origin], mpg)]

main :: IO ()
main = do
  putStrLn "=== Regress Library: Auto MPG Prediction ==="
  
  result <- loadAutoData "examples/auto.csv"
  case result of
    Left err -> putStrLn $ "Error loading data: " ++ err
    Right rawData -> do
      let normalizedData = normalizeFeatures rawData
          (trainData, testData) = trainTestSplit 0.8 normalizedData
          
          model = fit 0.01 15000 trainData
          
          trainRmse = rmse model trainData
          testRmse = rmse model testData
          testR2 = r2Score model testData
          testMae = mae model testData
          
          cvScore = crossValidate 5 0.01 15000 normalizedData

      putStrLn $ "Dataset: " ++ show (length rawData) ++ " samples"
      putStrLn $ "Features: cylinders, displacement, horsepower, weight, acceleration, year, origin"
      putStrLn $ "Target: MPG (fuel efficiency)"
      putStrLn ""
      putStrLn $ "Training samples: " ++ show (length trainData)
      putStrLn $ "Test samples: " ++ show (length testData)
      putStrLn ""
      putStrLn "=== Performance Metrics ==="
      putStrLn $ "Train RMSE: " ++ show trainRmse
      putStrLn $ "Test RMSE: " ++ show testRmse
      putStrLn $ "Test R²: " ++ show testR2
      putStrLn $ "Test MAE: " ++ show testMae
      putStrLn $ "5-fold CV MSE: " ++ show cvScore
      putStrLn ""
      
      putStrLn "=== Sample Predictions ==="
      let sampleFeatures = [8, 350, 165, 4000, 11.5, 72, 1]
      putStrLn $ "V8, 350 displacement, 165 HP, 4000 lbs, 11.5 acc, '72, USA → MPG: " 
      putStrLn $ show (predict model sampleFeatures)