module Opdr2 where

euclid :: Integer -> Integer -> Integer
euclid 0 y = y
euclid x y = euclid (y `mod` x) x

-- vragen
egcd' :: Integer -> Integer -> (Integer, Integer, Integer)
egcd' 0 b = (b, 0, 1)
egcd' a b =
  let (g, s, t) = egcd' (b `mod` a) a
   in (g, t - (b `div` a) * s, s)

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a b =
  let (g, s, t) = egcd' a b
   in if s < 0 then (g, s + b, t) else (g, s, t)

genkeys :: Integer -> Integer -> (Integer, Integer, Integer)
genkeys p q =
  let m = p * q
      m' = (p - 1) * (q - 1)
      e = head [x | x <- [2..m'], euclid x m' == 1]
      (_,d,_) = egcd e m'
   in (e, d, m)

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x = mod (x^e) m

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x = mod (x^d) m
