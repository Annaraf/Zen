module One where

import Data.List

-- 1a

--pembatas
--null' x

null' x
    | x == [] = True
    | otherwise = False


--pembatas
--take' x

take' _ [] = []
take' n (x:xs)
    | n == 0 = []
    | otherwise = [x] ++ (take' (n-1)(xs))


--pembatas
--drop' x

drop' 0 (x:xs) = (x:xs)
drop' _ [] = []
drop' n (x:xs) = (drop' (n-1) xs)

--pembatas
--fst' x

fst' (a,b) = a


--pembatas
--snd' x

snd' (a,b) = b


--pembatas
--map' x

map' n [] = []
map' n (x:xs) = [(n x)] ++ map' n xs

--pembatas
--filter' x


--pembatas
--delete' x

delete' _ [] = []
delete' n (x:xs)
    | n == x = xs
    | otherwise = [x] ++ (delete' n xs)


--pembatas
--deleteAll'


--pembatas
--foldl' x


--pembatas
--foldl1' x


--pembatas
--zip' x

chg' [n,m] = (n,m)

zip' [] _ = []
zip' _ [] = []
zip' (a:as) (x:xs) = [(chg' ([a] ++ [x]))] ++ (zip' (as) (xs))


--pembatas
--zipWith' x


--pembatas
--nth' x

nth' (x:xs) n = (head' (drop n (x:xs)))


--pembatas
--sort' x

sort' (x:xs) = (x:xs)


--pembatas
--scanl' x


--pembatas
--scanl1' x


--pembatas
--elem' x

elem' _ [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs
    

--pembatas
--notElem' x

notElem' _ [] = True
notElem' n (x:xs)
        | n == x = False
        | n /= x = notElem' n (xs)
        

--pembatas
--head' x 

head' (x:xs) = x


--pembatas
--length' x

length' [] = 0
length' (x:xs) = 1 + (length' xs )


--pembatas
--reverse' x

reverse' [] = []
reverse' (x:xs) = (reverse' (xs)) ++ [x]


--pembatas
--last' x

last' [n] = n
last' (x:xs) = last' (xs)


--pembatas
--tail' x

tail' (x:xs) = xs


--pembatas
--init' x

init' [n] = []
init' (x:xs) = [x] ++ (init' (xs))


--pembatas
--max' x

max' n m
  | n == m = n
  | n < m = m
  | n > m = n
  

--pembatas
--min' x

min' y z
  | y == z = y
  | y < z = y
  | y > z = z


--pembatas
--concat' x (unfinished function)

concat' [(x:xs)] = (x:xs)


--pembatas
--intersperse' x

intersperse' a [n] = [n]
intersperse' a (x:xs) =[x] ++ [a] ++ (intersperse' a (xs))


--pembatas
--inercalate' x (unfinished function)

intercalate' n [(x:xs)]
    | n == (x:xs) = (x:xs)


--pembatas
--and' x

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' (xs)


--pembatas
--or' x 

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' (xs)


--pembatas
--zip3' x

chg'' [a,b,c] = (a,b,c)

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (a:as) (b:bs) (c:cs) = [chg'' ([a] ++ [b] ++ [c])] ++ (zip3' (as) (bs) (cs))


--pembatas
--sum' x

sum' [] = 0
sum' (x:xs) = x + (sum (xs))

--pembatas
--product' x
product' [] = 1
product' (x:xs) = x * product' xs


--pembatas
-- words' x

words' x = x

--pembatas
--lines' x

lines' a = [a]


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
--all' x

all' _ [] = True
all' a (x:xs)
  | a x == False = False
  | otherwise = all' a (xs)
  

--pembatas
--any' 

any' _ [] = False
any' a (x:xs)
  | a x == True = True
  | otherwise = any' a (x:xs)


--pembatas
--insert' x

insert' n [] = [n]
insert' n (x:xs)
  | n <= x = [n] ++ (x:xs)
  | n > x = [x] ++ (insert' n (xs))

--pembatas

zipWith3' x = x


--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas
--minimum' x

minimum' [n] = n
minimum' (x:xs) = min' (x) (minimum' xs)


--pembatas
--maximum' x

maximum' [n] = n
maximum' (x:xs) = max' (x) (maximum' xs)


--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas
--union' x

union' (x:xs) (y:ys) = (x:xs) ++ (y:ys)


--pembatas

intersect' x = x

--pembatas
--group' x

group' [] = []
group' (x:xs) = [take 1 (x:xs)] ++ group' (xs)


--pembatas
--splitAt'

splitAt' _ [] = ([],[])
splitAt' n [m]
  | n == 0 = ([], [m])
splitAt' n (x:xs) = ((take n (x:xs)), (drop n (x:xs)))


--pembatas

partition' x = x

--pembatas

replicate' x = x






