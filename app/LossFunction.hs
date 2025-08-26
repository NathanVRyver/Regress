module LossFunction where

lossFunction :: [Double] -> [Double] -> Double
lossFunction predicted actual = sumSquaredErrors / fromIntegral (length predicted)
  where
    sumSquaredErrors = sum $ zipWith (\p a -> (p - a) ^ (2 :: Int)) predicted actual

