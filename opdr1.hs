module Opdr1 where

faca :: Int -> Int
faca 0 = 1
faca x = x * faca (x - 1)

facb :: Int -> Int
facb n
    | n == 0 = 1
    | otherwise = n * facb (n - 1)

nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c =
    let d = b ^ 2 - 4 * a * c
        root1 = (-b + sqrt d) / (2 * a)
        root2 = (-b - sqrt d) / (2 * a)
        root0 = -b / 2 * a
    in if d > 0 then [root1, root2]
       else if d == 0 then [root0]
       else []

nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | d > 0 = [root1, root2]
    | d == 0 = [root0]
    | otherwise = [] 
    where
        d = b ^ 2 - 4 * a * c
        root1 = (-b + sqrt d) / (2 * a)
        root2 = (-b - sqrt d) / (2 * a)
        root0 = -b / 2 * a

-- mult :: Integer -> Integer -> Integer
-- mult x y

-- fastmult :: Integer -> Integer -> Integer
-- fastmult x y

-- pow :: Integer -> Integer -> Integer
-- pow x y

-- fastpow :: Integer -> Integer -> Integer
-- fastpow x y
