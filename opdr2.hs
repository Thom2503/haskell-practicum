module Opdr2 where

euclid :: Integer -> Integer -> Integer
euclid 0 y = y
euclid x y = euclid (y `mod` x) x

-- vragen
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
        s' = if s < 0 then s `mod` g else s
        t' = if t < 0 then t `mod` g else t
    in (g, t' - (b `div` a) * s', s')

genkeys :: Integer -> Integer -> (Integer, Integer, Integer)
genkeys p q =
   let m = p * q
       m' = (p - 1) * (q - 1)
       e = 5 -- vragen
       d = (e ^ (-1)) * (e `mod` m')
    in (e, d, m)
