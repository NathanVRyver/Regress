{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (toList)
import Regress

data AdvertisingRow = AdvertisingRow Double Double Double Double

instance FromRecord AdvertisingRow where
  parseRecord v = AdvertisingRow <$> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4

loadAdvertisingData :: FilePath -> IO (Either String [Sample])
loadAdvertisingData filepath = do
  csvData <- BL.readFile filepath
  case decodeWith csvOptions HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ map toSample (toList records)
  where
    csvOptions = defaultDecodeOptions
    toSample (AdvertisingRow tv radio newspaper sales) = ([tv, radio, newspaper], sales)

main :: IO ()
main = do
  putStrLn "=== Regress Library Example: Advertising Dataset ==="
  
  result <- loadAdvertisingData "examples/advertising.csv"
  case result of
    Left err -> putStrLn $ "Error loading data: " ++ err
    Right rawData -> do
      let normalizedData = normalizeFeatures rawData
          (trainData, testData) = trainTestSplit 0.8 normalizedData
          
          model = fit 0.01 10000 trainData
          
          trainRmse = rmse model trainData
          testRmse = rmse model testData
          testR2 = r2Score model testData
          testMae = mae model testData
          
          cvScore = crossValidate 5 0.01 10000 normalizedData

      putStrLn $ "Dataset: " ++ show (length rawData) ++ " samples"
      putStrLn $ "Features: TV, Radio, Newspaper advertising spend"
      putStrLn $ "Target: Sales"
      putStrLn ""
      putStrLn $ "Training samples: " ++ show (length trainData)
      putStrLn $ "Test samples: " ++ show (length testData)
      putStrLn ""
      putStrLn $ "Model: " ++ show model
      putStrLn ""
      putStrLn "=== Performance Metrics ==="
      putStrLn $ "Train RMSE: " ++ show trainRmse
      putStrLn $ "Test RMSE: " ++ show testRmse
      putStrLn $ "Test R²: " ++ show testR2
      putStrLn $ "Test MAE: " ++ show testMae
      putStrLn $ "5-fold CV MSE: " ++ show cvScore
      putStrLn ""
      
      putStrLn "=== Sample Predictions ==="
      let sampleFeatures = [200.0, 30.0, 50.0]
      putStrLn $ "TV=200, Radio=30, Newspaper=50 → Sales: " 
      putStrLn $ show (predict model sampleFeatures)