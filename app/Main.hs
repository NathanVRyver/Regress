module Main where

import Optimizer (Sample, Params, predict, trainGD)
import DataLoader (loadF1Data, normalizeFeatures)
import Metrics (r2Score, mae, rmse, trainTestSplit, crossValidate)

runF1LapTimePredictor :: IO ()
runF1LapTimePredictor = do
  putStrLn "=== F1 Lap Time Prediction Model ==="
  
  result <- loadF1Data "f1_data.csv"
  case result of
    Left err -> putStrLn $ "Error loading data: " ++ err
    Right rawData -> do
      let normalizedData = normalizeFeatures rawData
          (trainData, testData) = trainTestSplit 0.8 normalizedData
          
          p0 :: Params
          p0 = ([0, 0, 0, 0], 0)
          
          steps = 10000
          lr = 0.01
          
          trainedParams = trainGD steps lr p0 trainData
          
          trainMse = rmse trainedParams trainData
          testMse = rmse trainedParams testData
          testR2 = r2Score trainedParams testData
          testMae = mae trainedParams testData
          testRmse = rmse trainedParams testData
          
          cvScore = crossValidate 3 normalizedData trainGD steps lr p0

      putStrLn $ "Training samples: " ++ show (length trainData)
      putStrLn $ "Test samples: " ++ show (length testData)
      putStrLn $ "Features: driver_experience, tire_compound, fuel_load, track_temp"
      putStrLn ""
      putStrLn $ "Final params: " ++ show trainedParams
      putStrLn $ "Train RMSE: " ++ show trainMse
      putStrLn $ "Test RMSE: " ++ show testMse  
      putStrLn $ "Test R²: " ++ show testR2
      putStrLn $ "Test MAE: " ++ show testMae
      putStrLn $ "Test RMSE: " ++ show testRmse
      putStrLn $ "3-fold CV MSE: " ++ show cvScore
      putStrLn ""
      
      putStrLn "=== Predictions vs Actual (Test Set) ==="
      let predictions = map (predict trainedParams . fst) testData
          actuals = map snd testData
      mapM_ (\(p, a) -> putStrLn $ "Predicted: " ++ show (round p :: Int) ++ "s, Actual: " ++ show (round a :: Int) ++ "s") 
            (zip predictions actuals)
      
      putStrLn ""
      let sampleFeatures = [12.5, 1.0, 42.0, 30.0]
      putStrLn $ "Sample prediction (exp=12.5, tire=1, fuel=42, temp=30): " 
      putStrLn $ show (predict trainedParams sampleFeatures) ++ " seconds"

main :: IO ()
main = runF1LapTimePredictor

