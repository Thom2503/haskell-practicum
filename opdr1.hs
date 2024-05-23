module Opdr1 where

faca :: Int -> Int
faca 0 = 1
faca x = x * faca (x - 1)

facb :: Int -> Int
facb n
    | n == 0 = 1
    | otherwise = n * facb (n - 1)

-- nulpuntena :: Double -> Double -> Double -> [Double]
-- nulpuntena a b c


-- nulpuntenb :: Double -> Double -> Double -> [Double]
-- nulpuntenb a b c

-- mult :: Integer -> Integer -> Integer
-- mult x y

-- fastmult :: Integer -> Integer -> Integer
-- fastmult x y

-- pow :: Integer -> Integer -> Integer
-- pow x y

-- fastpow :: Integer -> Integer -> Integer
-- fastpow x y
