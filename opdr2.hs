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


-- Oefenen met genkeys rsaencrypt en rsadecrypt
--ghci> import Data.Char
--ghci> genkeys 7 67
--(5,317,469)
--ghci> ord 'k'
--107
--ghci> rsaencrypt (5,469) 107
--81
--ghci> rsadecrypt (317,469) 81
--107
--ghci> chr 81
--'Q'
--ghci> chr 107
--'k'


rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x = mod (x^e) m

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x = mod (x^d) m

-- Opdracht 5
-- Als Alice een bericht veilig wil sturen naar Bob kan zij het beste,
-- eerst het bericht met de public key van bob encrypten en dan met haar
-- eigen private key encrypten. En dan aan de kant van Bob moet bob eerst de public
-- key van Alice gebruiken om het te decrypten en dan zijn eigen private key gebruiken
-- om het nog een keer te decrypten.
