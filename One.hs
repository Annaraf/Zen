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

filter' _ [] = []
filter' a (x:xs)
  | a x == True = [x] ++ filter' a (xs)
  | otherwise = filter' a (xs)


--pembatas
--delete' x

delete' _ [] = []
delete' n (x:xs)
    | n == x = xs
    | otherwise = [x] ++ (delete' n xs)


--pembatas
--deleteAll'

deleteAll' _ [] = []
deleteAll' n (x:xs)
    | n == x = deleteAll' n (xs)
    | otherwise = [x] ++ (deleteAll' n xs)


--pembatas
--foldl' x

foldlll' n m [] = m
foldlll' n m (x:xs) = n (foldlll' n m xs) (x)


--pembatas
--foldl1' x

foldll1' n [m] = m
foldll1' n (x:xs) = n (foldll1' n xs) (x)


--pembatas
--zip' x

zipp' [] _ = []
zipp' _ [] = []
zipp' (x:xs) (y:ys) = [(x,y)] ++ zipp' xs ys


--pembatas
--zipWith' x

zipWith' n _ [] = []
zipWith' n [] _ = []
zipWith' n (x:xs) (y:ys) = [(n x y)] ++ (zipWith' n xs ys)


--pembatas
--nth' x

nth' (x:xs) n = (head' (drop n (x:xs)))



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

concat' [] = []
concat' [a] = a
concat' (n:m) = n ++ concat' m

--pembatas
--intersperse' x

intersperse' a [n] = [n]
intersperse' a (x:xs) =[x] ++ [a] ++ (intersperse' a (xs))


--pembatas
--inercalate' x 

intercalate' a [n] = n
intercalate' a (n:m) = n ++ a ++ (intercalate' a m)


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

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (a:as) (b:bs) (c:cs) = [(a,b,c)] ++ (zip3' (as) (bs) (cs))


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
--takeWhile' x

takeWhile' n (x:xs)
  | n x == False = []
  | n x == True = [x] ++ takeWhile' n (xs)


--pembatas
--dropWhile' x

dropWhile' _ [] = []
dropWhile' n (x:xs)
  | n x == False = (x:xs)
  | n x == True = dropWhile' n xs


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

--pembatas
--nub' x 

nub' [] = []
nub' [n] = [n]
nub' (x:xs) = [x] ++ (nub' (deleteAll' x (x:xs)))


--pembatas
--sort' x

sort' [] = []
sort' [n] = [n]
sort' (x:xs)
  | x == minimum (x:xs) = [x] ++ (sort' xs)
  | x > (head xs)  = sort' (xs ++ [x])
  | otherwise = sort' (xs ++ [x])


minimum' [n] = n
minimum' (x:xs) = min' (x) (minimum' xs)


--pembatas
--maximum' x

maximum' [n] = n
maximum' (x:xs) = max' (x) (maximum' xs)


--pembatas
--inits' x

inits' [] = [[]]
inits' (x:xs) = (inits' (init (x:xs))) ++ [(x:xs)]


--pembatas
--tails' x

tails' [] = [[]]
tails' (x:xs) = [take (length (x:xs)) (x:xs)] ++ tails' xs


--pembatas
--union' x

union' (x:xs) (y:ys) = (x:xs) ++ (y:ys)


--pembatas
--intersect' x

intersect' [] _ = []
intersect' (x:xs) (y:ys)
  | x == y = [x] ++ intersect' xs ys
  | x /= y = intersect' xs (y:ys)


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
--replicate' x

replicate' n a
  | n == 0 = []
  | otherwise = [a] ++ replicate' (n-1) a





