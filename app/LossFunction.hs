module LossFunction where
-- mean squared error
lossFunction :: [Double] -> [Double] -> Double
lossFunction [] [] = 0
lossFunction (x:xs) (y:ys) =
  let numerator   = (x - y) ^ 2 + fst (recurse xs ys)
      denominator = 1 + snd (recurse xs ys)
  in numerator / fromIntegral denominator
  where
    recurse [] [] = (0, 0)  
    recurse (a:as) (b:bs) =
      let (sumRest, countRest) = recurse as bs
      in ((a - b) ^ 2 + sumRest, 1 + countRest)

