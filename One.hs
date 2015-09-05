module One where

import Data.List

-- 1a

--null' x

null' x
    | x == [] = True
    | otherwise = False


--take' x

take' n (x:xs)
    | n > length' (x:xs) = (x:xs)
    | n == 0 = []
    | otherwise = [x] ++ (take' (n-1)(xs))


--drop' x

drop' n (x:xs)
    | n >= length' (x:xs) = []
    | n == 0 = (x:xs)
    | otherwise = (drop (n-1) xs)


--fst' x

fst' (a,b) = a


--snd' x

snd' (a,b) = b

--map' x


--filter' x


--delete' x

delete' _ [] = []
delete' n (x:xs)
    | n == x = xs
    | otherwise = [x] ++ (delete' n xs)


--deleteAll'


--foldl' x


--foldl1' x


--zip' x

chg' [n,m] = (n,m)

zip' [] _ = []
zip' _ [] = []
zip' (a:as) (x:xs) = [(chg' ([a] ++ [x]))] ++ (zip' (as) (xs))


--zipWith' x


--nth' x


--sort' x

sort' (x:xs) = (x:xs)


--scanl' x


--scanl1' x


--elem' x


--notElem' x


--head' x 

head' (x:xs) = x


--length' x

length' [] = 0
length' (x:xs) = 1 + (length' xs )


--reverse' x

reverse' [] = []
reverse' (x:xs) = (reverse' (xs)) ++ [x]


--last' x

last' [n] = n
last' (x:xs) = last' (xs)


--tail' x

tail' (x:xs) = xs


--init' x

init' [n] = []
init' (x:xs) = [x] ++ (init' (xs))

--max' x

max' n m
  | n == m = n
  | n < m = m
  | n > m = n
  

--min' x

min' y z
  | y == z = y
  | y < z = y
  | y > z = z


--concat' x

concat' [(x:xs)] = (x:xs)


--intersperse' x

intersperse' a [n] = [n]
intersperse' a (x:xs) =[x] ++ [a] ++ (intersperse' a (xs))


--inercalate' x

intercalate' n [(x:xs)]
    | n == (x:xs) = (x:xs)


--and' x

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' (xs)


--or' x 

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' (xs)


--zip3' x


--sum' x


--product' x
product' [] = 1
product' (x:xs) = x * product' xs




--unfinished function
--unfinished function
--unfinished function


words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' x = x

--pembatas

dropWhile' x = x

--pembatas

concatMap' x = x

--pembatas

all' x = x

--pembatas

any' x = x

--pembatas

insert' x = x

--pembatas

zipWith3' x = x

--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas

minimum' x = x

--pembatas

maximum' x = x

--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' x = x

--pembatas

splitAt' x = x

--pembatas

partition' x = x

--pembatas

replicate' x = x






