module One where

import Data.List

-- 1a
-- masih ada yang salah :)

null' x
    | x == [] = True
    | otherwise = False

length' [] = 0
length' (x:xs) = 1 + (length' xs )

take' n (x:xs)
    | n > length' (x:xs) = (x:xs)
    | n == 0 = []
    | otherwise = [x] ++ (take' (n-1)(xs))

drop' n (x:xs)
    | n >= length' (x:xs) = []
    | n == 0 = (x:xs)
    | otherwise = (drop (n-1) xs)

elem' n (x:xs)
    | n == x = True
    | n == [] = False
    | otherwise = elem' n xs

delete' _ [] = []
delete' n (x:xs)
    | n == x = xs
    | otherwise = [x] ++ (delete' n xs)

fst' (a,b) = a

snd' (a,b) = b

concat' [(x:xs)] = (x:xs)

intercalate' n [(x:xs)]
    | n == (x:xs) = (x:xs)

product' [] = 1
product' (x:xs) = x * product' xs

head' (x:xs) = x

chg' [n,m] = (n,m)

zip' [] _ = []
zip' _ [] = []
zip' (a:as) (x:xs) = [(chg' ([a] ++ [x]))] ++ (zip' (as) (xs))

sort' (x:xs) = (x:xs)

max' n m
  | n == m = n
  | n < m = m
  | n > m = n

min' y z
  | y == z = y
  | y < z = y
  | y > z = z

tail' (x:xs) = xs

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' (xs)

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' (xs)
  
