module Opdr3 where

differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

-- partieel parametiseren zoals:
-- ghci> x = Opdr3.differentieer (\x -> x ^ 2) (1 / 1000)
-- ghci> x 10
-- 20.000999999993496
-- ghci> x 340
-- 680.0009999860777
-- ghci>

integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = 
  let n = (a - b) / p
      dx = (a - b) / n
   in dx * sum [f (a + i * dx) | i <- [0..(n - 1)]]