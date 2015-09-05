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

map' (n) m [] = []
map' (n) m (x:xs) = [(n) m x] ++ map' (n) m (xs)

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
--concat' x

concat' [(x:xs)] = (x:xs)


--pembatas
--intersperse' x

intersperse' a [n] = [n]
intersperse' a (x:xs) =[x] ++ [a] ++ (intersperse' a (xs))


--pembatas
--inercalate' x

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
--insert' x

insert' n [] = [n]
insert' n (x:xs)
  | n <= x = [n] ++ (x:xs)
  | n > x = [x] ++ (insert' n (xs))

--pembatas

zipWith3' x = x



--unfinished function
--unfinished function
--unfinished function
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






