module Opdr3 where

{--
1055805 Veldhuis, Thom
1059179 Zumker, Douwe
--}


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
dubbelen :: (Ord a, Eq a) => [a] -> [a]
dubbelen s = [head x | x <- fst $ partition (\x -> length x > 1) $ group s]

-- POKEREN --
frequencies :: (Ord a, Eq a) => [a] -> [(a, Int)]
frequencies xs = map (\x -> (head x, length x)) $ group $ sort xs

ofAKind :: [Int] -> Int -> Bool
ofAKind xs n = any (\ (_, count) -> count >= n) $ frequencies xs

isStraight :: [Int] -> Bool
isStraight xs = sort xs `elem` [[1,2,3,4,5], [2,3,4,5,6]]

isNPair :: [Int] -> Int -> Bool
isNPair xs n = (length $ filter (== 2) (map snd (frequencies xs))) == n

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
    | isNPair dice 2 = "Two Pair"
    | isNPair dice 1 = "One Pair"
    | otherwise = "Bust"

throws :: [[Int]]
throws = [[a,b,c,d,e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]

determineThrows :: [(String, Double)]
determineThrows = sortOn snd $ map (\ (x, y) -> (x, (fromIntegral y / fromIntegral (length throws)) * 100)) $ frequencies [determine x | x <- throws]