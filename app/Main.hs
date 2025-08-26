module Main where

import Optimizer (Sample, Params, predict, mse, trainGD)

runLinearRegression :: IO ()
runLinearRegression = do
  let ds :: [Sample]
      ds = [ ([1,  0],  7)
           , ([0,  1],  2)
           , ([2,  1],  6)
           , ([3, -1], 16)
           , ([4,  2],  7)
           ]

      p0 :: Params
      p0 = ([0, 0], 0)

      steps = 5000
      lr = 0.01
      loss0 = mse p0 ds
      pFinal = trainGD steps lr p0 ds
      lossFinal = mse pFinal ds

  putStrLn "=== Linear Regression via Gradient Descent ==="
  putStrLn $ "init params: " ++ show p0
  putStrLn $ "init loss  : " ++ show loss0
  putStrLn $ "steps      : " ++ show steps
  putStrLn $ "lr         : " ++ show lr
  putStrLn $ "final params: " ++ show pFinal
  putStrLn $ "final loss  : " ++ show lossFinal

  let xTest = [2, -3]
  putStrLn $ "predict([2,-3]) = " ++ show (predict pFinal xTest)

main :: IO ()
main = runLinearRegression

