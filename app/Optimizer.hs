module Optimizer where

import LossFunction (lossFunction)

-- Types
type Vector = [Double]                 
type Sample = (Vector, Double)        
type Params = (Vector, Double)       

-- ---------- Model ----------
predict :: Params -> Vector -> Double
predict (w, b) x = b + sum (zipWith (*) w x)

-- ---------- Loss (MSE) ----------
mse :: Params -> [Sample] -> Double
mse p ds = lossFunction predictions actuals
  where
    predictions = map (predict p . fst) ds
    actuals = map snd ds

-- ---------- Gradients of MSE ----------
-- dL/db   = (2/m) * Σ e_i
-- dL/dw_j = (2/m) * Σ e_i * x_{i,j}
grads :: Params -> [Sample] -> (Vector, Double)
grads (w, b) ds = (map (/m2) gWsum, gBsum / m2)
  where
    (gWsum, gBsum, n) = foldr step (replicate (length w) 0, 0, 0 :: Int) ds
    step (x,y) (gw, gb, k) =
      let e   = predict (w,b) x - y
          gw' = zipWith (\gj xj -> gj + e * xj) gw x
          gb' = gb + e
      in (gw', gb', k+1)
    m2 = fromIntegral n * 2.0

-- ---------- One GD step ----------
-- Inputs: learning rate, current params, dataset
-- Output: updated params
gdStep :: Double -> Params -> [Sample] -> Params
gdStep eta (w, b) ds =
  let (gW, gB) = grads (w, b) ds
      w' = zipWith (\wj gj -> wj - eta * gj) w gW
      b' = b - eta * gB
  in (w', b')

-- ---------- Training loop (T steps) ----------
-- Inputs: steps, learning rate, initial params, dataset
-- Output: optimized params
trainGD :: Int -> Double -> Params -> [Sample] -> Params
trainGD 0 _    p _  = p
trainGD t eta p ds  = trainGD (t-1) eta (gdStep eta p ds) ds

