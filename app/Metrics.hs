module Metrics where

import Optimizer (Sample, Params, predict)
import LossFunction (lossFunction)

r2Score :: Params -> [Sample] -> Double
r2Score params samples = 1 - (ss_res / ss_tot)
  where
    predictions = map (predict params . fst) samples
    actuals = map snd samples
    actualMean = sum actuals / fromIntegral (length actuals)
    ss_res = sum $ zipWith (\p a -> (a - p) ^ (2 :: Int)) predictions actuals
    ss_tot = sum $ map (\a -> (a - actualMean) ^ (2 :: Int)) actuals

mae :: Params -> [Sample] -> Double
mae params samples = meanAbsError / fromIntegral (length samples)
  where
    predictions = map (predict params . fst) samples
    actuals = map snd samples
    meanAbsError = sum $ zipWith (\p a -> abs (a - p)) predictions actuals

rmse :: Params -> [Sample] -> Double
rmse params samples = sqrt $ lossFunction predictions actuals
  where
    predictions = map (predict params . fst) samples
    actuals = map snd samples

trainTestSplit :: Double -> [Sample] -> ([Sample], [Sample])
trainTestSplit ratio samples = (take trainSize samples, drop trainSize samples)
  where
    trainSize = round $ ratio * fromIntegral (length samples)

crossValidate :: Int -> [Sample] -> (Int -> Double -> Params -> [Sample] -> Params) -> Int -> Double -> Params -> Double
crossValidate k samples trainFunc steps lr initialParams = 
  let foldSize = length samples `div` k
      folds = createFolds k samples
      scores = map (validateFold trainFunc steps lr initialParams) folds
  in sum scores / fromIntegral k

createFolds :: Int -> [a] -> [([a], [a])]
createFolds k items = 
  let foldSize = length items `div` k
  in [ let (train1, rest) = splitAt (i * foldSize) items
           (test, train2) = splitAt foldSize rest
       in (train1 ++ train2, test)
     | i <- [0..k-1]
     ]

validateFold :: (Int -> Double -> Params -> [Sample] -> Params) -> Int -> Double -> Params -> ([Sample], [Sample]) -> Double
validateFold trainFunc steps lr initialParams (trainSet, testSet) =
  let trainedParams = trainFunc steps lr initialParams trainSet
  in lossFunction (map (predict trainedParams . fst) testSet) (map snd testSet)