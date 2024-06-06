module Opdr3 where

import Data.List

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

-- ghci> x = Opdr3.integreer (\x -> x ^ 3) (-2) 2
-- ghci> x 1
-- 0.0

-- WERKEN MET BIBLIOTHEKEN --
-- vragen mogen wij de signature aanpassen
-- dubbelen :: [a] -> [a] 
dubbelen :: (Ord a, Eq a) => [a] -> [a]
dubbelen s = [head x | x <- fst $ partition (\x -> length x > 1) $ group s]

-- POKEREN --
frequencies :: [Int] -> [(Int, Int)]
frequencies xs = map (\x -> (head x, length x)) $ group $ sort xs

ofAKind :: [Int] -> Int -> Bool
ofAKind xs n = any (\ (_, count) -> count >= n) $ frequencies xs

isStraight :: [Int] -> Bool
isStraight xs = sort xs `elem` [[1,2,3,4,5], [2,3,4,5,6]]

isTwoPair :: [Int] -> Bool
isTwoPair xs = (length $ filter (== 2) (map snd (frequencies xs))) == 2

isFullHouse :: [Int] -> Bool
isFullHouse xs =
    let freqs = map snd (frequencies xs) in
       sort freqs == [2, 3]

determine :: [Int] -> String
determine dice
    | ofAKind dice 5 = "Poker"
    | ofAKind dice 4 = "Four of a Kind"
    | isFullHouse dice = "Full House"
    | ofAKind dice 3 = "Three of a Kind"
    | isStraight dice = "Straight"
    | isTwoPair dice = "Two Pair"
    | ofAKind dice 1 = "One Pair"
    | otherwise = "Bust"

-- vragen
probability :: ([Int] -> Bool) -> [[Int]] -> Double
probability func xs = (fromIntegral $ (length $ filter func xs)) / (fromIntegral $ length xs)