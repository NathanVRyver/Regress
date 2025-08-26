module Regress.Metrics
  ( r2Score
  , mae
  , rmse
  , trainTestSplit
  , crossValidate
  ) where

import Regress.Core (Model, Sample, predict, mse, fit)

r2Score :: Model -> [Sample] -> Double
r2Score model samples = 1 - (ss_res / ss_tot)
  where
    predictions = map (predict model . fst) samples
    actuals = map snd samples
    actualMean = sum actuals / fromIntegral (length actuals)
    ss_res = sum $ zipWith (\p a -> (a - p) ^ (2 :: Int)) predictions actuals
    ss_tot = sum $ map (\a -> (a - actualMean) ^ (2 :: Int)) actuals

mae :: Model -> [Sample] -> Double
mae model samples = meanAbsError / fromIntegral (length samples)
  where
    predictions = map (predict model . fst) samples
    actuals = map snd samples
    meanAbsError = sum $ zipWith (\p a -> abs (a - p)) predictions actuals

rmse :: Model -> [Sample] -> Double
rmse model samples = sqrt $ mse model samples

trainTestSplit :: Double -> [Sample] -> ([Sample], [Sample])
trainTestSplit ratio samples = (take trainSize samples, drop trainSize samples)
  where
    trainSize = round $ ratio * fromIntegral (length samples)

crossValidate :: Int -> Double -> Int -> [Sample] -> Double
crossValidate k lr maxIter samples = 
  let folds = createFolds k samples
      scores = map (validateFold lr maxIter) folds
  in sum scores / fromIntegral k

createFolds :: Int -> [a] -> [([a], [a])]
createFolds k items = 
  let foldSize = length items `div` k
  in [ let (train1, rest) = splitAt (i * foldSize) items
           (test, train2) = splitAt foldSize rest
       in (train1 ++ train2, test)
     | i <- [0..k-1]
     ]

validateFold :: Double -> Int -> ([Sample], [Sample]) -> Double
validateFold lr maxIter (trainSet, testSet) =
  let trainedModel = fit lr maxIter trainSet
  in mse trainedModel testSet