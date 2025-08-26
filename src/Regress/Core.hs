module Regress.Core
  ( Vector
  , Sample
  , Model(..)
  , predict
  , fit
  , mse
  ) where

type Vector = [Double]
type Sample = (Vector, Double)

data Model = Model
  { weights :: Vector
  , bias :: Double
  , learningRate :: Double
  , iterations :: Int
  } deriving (Show, Eq)

predict :: Model -> Vector -> Double
predict (Model w b _ _) x = b + sum (zipWith (*) w x)

mse :: Model -> [Sample] -> Double
mse model samples = sumSquaredErrors / fromIntegral (length samples)
  where
    predictions = map (predict model . fst) samples
    actuals = map snd samples
    sumSquaredErrors = sum $ zipWith (\p a -> (p - a) ^ (2 :: Int)) predictions actuals

fit :: Double -> Int -> [Sample] -> Model
fit lr maxIter samples = trainGD maxIter initialModel samples
  where
    numFeatures = length . fst . head $ samples
    initialModel = Model (replicate numFeatures 0) 0 lr maxIter

trainGD :: Int -> Model -> [Sample] -> Model
trainGD 0 model _ = model
trainGD n model samples = trainGD (n-1) updatedModel samples
  where
    updatedModel = gdStep model samples

gdStep :: Model -> [Sample] -> Model
gdStep (Model w b lr iter) samples = Model w' b' lr iter
  where
    (gW, gB) = gradients (Model w b lr iter) samples
    w' = zipWith (\wj gj -> wj - lr * gj) w gW
    b' = b - lr * gB

gradients :: Model -> [Sample] -> (Vector, Double)
gradients model samples = (map (/m2) gWsum, gBsum / m2)
  where
    (gWsum, gBsum, n) = foldr step (replicate (length (weights model)) 0, 0, 0 :: Int) samples
    step (x, y) (gw, gb, k) =
      let e = predict model x - y
          gw' = zipWith (\gj xj -> gj + e * xj) gw x
          gb' = gb + e
      in (gw', gb', k+1)
    m2 = fromIntegral n * 2.0