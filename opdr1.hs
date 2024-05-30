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

worpen :: [(Integer, Integer, Integer)]
worpen = [(x, y, z)| x <- [1..6], y <- [1..6], z <- [1..6]]

inputalsworpen :: [(Integer, Integer, Integer)]
inputalsworpen = filter (\ (x, y, z) -> (x + y + z) `mod` 5 == 0) worpen
-- de grootte van de lijst met de worpen mod 5 is 43
grootte :: Int
grootte = length inputalsworpen

inputalsworpend :: Integer -> [(Integer, Integer, Integer)]
inputalsworpend n = filter (\ (x, y, z) -> (x + y + z) `mod` n == 0) worpen

grootted :: Integer -> Int
grootted n = length $ inputalsworpend n

-- :/ 0 0 0
puzzle = [(x, y, z)| x <- [-100..100], y <- [-100..100], z <- [-100..100],
           x == eerste y z && y == tweede x z && z == derde x y]

eerste y z = (y - z) * 2
tweede x z = x * z
derde x y = (x + y) / 2

-- ergens tussen de 100000 en de 10000000
-- want:
-- ghci> Opdr1.mult 1000000 1000000
--       1000000000000
-- ghci> Opdr1.mult 10000000 10000000
--       *** Exception: stack overflow
-- Met wat veranderingen geeft onze code die we hier hebben deze resultaten
-- ghci> Opdr1.eerstestack
--       [81000000000000,81000009000000,*** Exception: stack overflow
-- als we het ongeveer checken lijkt het met deze getallen te gebeuren
-- ghci> Opdr1.mult 9500000 9500000
--       *** Exception: stack overflow
-- ghci> Opdr1.mult 9000000 9000000
--       81000000000000
eerstestack = [mult x y | x <- [9500000..10000000], y <- [9500000..10000000]]

mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult x y = x + mult x (y - 1)

-- Dit gaat tot ontiegelijk ver veel verder dan het getal bij mult
fastmult :: Integer -> Integer -> Integer
fastmult _ 0 = 0
fastmult 0 _ = 0
fastmult x y
    | odd minnum = maxnum + fastmult (minnum - 1) maxnum
    | even minnum = fastmult (minnum `div` 2) (maxnum * 2)
    where
        minnum = min x y
        maxnum = max x y

-- Wanneer kregen we een Exception
-- pow 10000000 10000000
-- *** Exception: stack overflow
-- Als we pow 1999999 1999999 deden kregen we geen exception
-- Maar het duurde heel erg lang voordat we een antwoord eruit kregen
pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow x 1 = x
pow x y = x * pow x (y - 1)

-- ditto als fastmult
fastpow :: Integer -> Integer -> Integer
fastpow x 0 = 1
fastpow x y = fastpow' x y 1
    where
        fastpow' basis exp tell
            | exp == 0 = tell
            | even exp = fastpow' (basis * basis) (exp `div` 2) tell
            | odd exp = fastpow' (basis * basis) ((exp - 1) `div` 2) (basis * tell)
