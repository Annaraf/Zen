module ZenLeague where

import Data.List


add' x 0 = x
add' x y = add' (succ x) (pred y)

mult 0 y = 0
mult x 0 = 0
mult x y = x + mult x (pred y)

div' x 1 = x
div' x y
  | x < y = 0
  | otherwise = 1 + div (x-y) y




rem' n x
  | n == x = 0
  | n < x = n
  | n > x =  rem (n-x) x

numToDigit n
  | n < 10 = [n]
  | otherwise = numToDigit (div n 10)  ++ [rem n 10 ]

digitToNum [p] = p
digitToNum (x:xs)
   | otherwise = ((10^((length (x:xs)) - 1)) * x) + digitToNum xs

digitToNum' [p] = p
digitToNum' (x:xs) = ((4^((length (x:xs)) - 1)) * x) + digitToNum' xs

--kasur ini rusak
kasurRusak [] = 0
kasurRusak (x:xs)
  | reverse (numToDigit x) == numToDigit x = 1 + kasurRusak xs
  |otherwise = kasurRusak xs

jumlahKasurRusak [] = []
jumlahKasurRusak (x:xs)
  | reverse (numToDigit x) == numToDigit x = [x] ++ jumlahKasurRusak xs
  |otherwise = jumlahKasurRusak xs



ngitungNumToDigit [] = []
ngitungNumToDigit (x:xs) = numToDigit' x ++ ngitungNumToDigit xs


numToDigitt n
    | n < 10 = faktoriall n
    | otherwise = numToDigitt (div n 10)  + faktoriall (rem n 10)
          where faktoriall n
                  | n == 0 = 1
                  | n > 0 = n * (faktoriall (n-1))
--numToDigitt 123
-- = numToDigitt (div 123 10) + faktorialAna (rem 123 10)
-- = numToDigitt 12 + faktorialAna 3
-- = numToDigitt (div 12 10) ++



--fungsii 20 = 6765
--fungsii 21 = 10946
fungsii 1 = 1
fungsii 2 = 1
fungsii n = fungsii (pred n) + fungsii (pred (pred n))

--13

fibo (x:xs) = [x] ++ (fibo (sum (x:xs): [x]))

--20
nyariFibo [] = 0
nyariFibo (x:xs)
  | div x (10^1199) /= 0 = 1 + nyariFibo xs
  | otherwise = nyariFibo xs

--15
trimo 1 = 1
trimo 2 = 2
trimo 3 = 3
trimo n = trimo (pred n) + trimo (pred (pred n)) + trimo (pred(pred(pred n)))

trimoT [a,b,c] = [c] ++ trimoT [b,c,(a+b+c)]
--trimo [1,1,3] = [3] ++ trimo [1,3,5]
--              = [3] ++ [5] ++ trimo [3,5,9]
--              = [3] ++ [5] ++ [9] ++ trimo [5,9,17]


--18

faktorFaktor n [] = []
faktorFaktor n (x:xs)
    | rem n x == 0 = [x] ++ faktorFaktor n xs
    | otherwise = faktorFaktor n xs


urutanFaktor 1 = [[1]]
urutanFaktor n = [faktorFaktor n [1..n]]  ++ (urutanFaktor (n-1))


maximum' [b] = length b
maximum' (x:xs) = max' (length x) (maximum' xs)
   where max' n m
           | n == m = n
           | n < m = m
           | n > m = n
--50
--faktr lebih cepat :D

faktorFaktorT n [] = []
faktorFaktorT n (x:xs)
  | rem n x == 0 = [x] ++ [div n x] ++ faktorFaktorT n xs
  | otherwise = faktorFaktorT n xs

tambahFraksi [] [] = []
tambahFraksi (x:xs) (y:ys) = [[x * (2143000903680000 / y)]] ++ tambahFraksi xs ys

--31

--jumlahAngka n = div n (10^6)

--38

polinom x = (34* (x^16)) + (43* (x^13)) + (23* (x^12)) + (20* (x^10)) + (12* (x^9)) + (234* (x^5)) + (34* (x^3)) + (54* (x^2))

--33

primaFaktor [] = []
primaFaktor (x:xs)
  | x == primaT x [2..(x-1)] = [x] ++ primaFaktor xs
  | otherwise = primaFaktor xs

--48


--21
primaFibo [] = 0
primaFibo (x:xs)
  | x == primaT x [2..(x-1)] = x + primaFibo xs
  | otherwise = primaFibo xs

primaT n [] = n
primaT n (x:xs)
  | rem n x == 0 = 0
  | otherwise = primaT n xs



--25
nyariPrimaT 10000000 = []
nyariPrimaT n = primaT' n [2..(div n 2)] ++ (nyariPrimaT (n+1))
   where primaT' n [] = [n]
         primaT' n (x:xs)
           | rem n x == 0 = []
           | otherwise = primaT' n xs
--142858

--56

numToDigit' n
    | n < 2 = [n]
    | otherwise = numToDigit' (div n 2)  ++ [(rem n 2) ]


--142858

srimeTujuh x = sum ( map (7 *) (nyariPrimaT x))

--55

primaKe n m
  | ((n == 1) && (m == (primaT m [2..(div m 2)]))) = m
  | (m == (primaT m [2..(div m 2)])) = primaKe (n-1) (m+1)
  | (m /= (primaT m [2..(div m 2)])) = primaKe n (m+1)

primaKee n (x:xs)
  | n == 1 = x
  | otherwise = primaKee (n-1) xs

primaKeT n
  | n == 1 = 2
  | n /= 1 = last (take n (nyariPrimaT 2))


dobelPrima n = (primaKe (primaKe n 2) 2)



--dobelPrimaT n = primaKe (n + (primaKe n 2) - (primaKe (n-1) 2)) 2

--dobelPrimaT 4 = primaKe ( 4 + 7 - )

--jumlahDobelPrima 1000000000 = 0
jumlahDobelPrima m n = dobelPrima m + dobelPrima n

--primaKe 3 2
--prikake 2  3
--primaKe 1 4


--25 (rumusnya masih salah)

srimeTiga c (x:xs)
  | rem c 7 /= 0 = []
  | rem c 7 == 0 = primaT' c (delete 7 (x:xs))
     where primaT' n [] = [n]
           primaT' n (x:xs)
              | rem n x == 0 = []
              | otherwise = primaT' n xs

nyariSrimeTiga [] = []
nyariSrimeTiga (a:as) = (srimeTiga a [2..(div a 2)]) ++ (nyariSrimeTiga (as))

srimeEmpat c [] = []
srimeEmpat c (x:xs)
   | rem c x == 0 = srimeTiga c xs
   | rem c x /= 0 = srimeEmpat c xs

nyariSrimeEmpat [] = []
nyariSrimeEmpat (a:as) = (srimeEmpat a [2..(div a 2)]) ++ (nyariSrimeEmpat (as))

--aplikasi rumus antara [5..1000000]
--ternyata rumusnya masih salah wakakkaka


--14

jumlahPrima 1 (x:xs) = 0
jumlahPrima n (x:xs) = prima n (x:xs) + jumlahPrima (n-1) (x:xs)


prima b (x:xs)
  | length (uhuy 0 ( map (rem b) (x:xs))) == 2 = b
  | otherwise = 0


uhuy m [] = []
uhuy m (x:xs)
   | m == x = [x] ++ uhuy m xs
   | m /= x = uhuy m xs

--22
srimes b [] = 0
srimes b (x:xs)
     | length (uhuy 0 ( map (rem b) (x:xs))) == 3 = 1
     | length (uhuy 0 ( map (rem b) (x:xs))) == 4 = 1
     | otherwise = 0

jumlahSrimes 999 (x:xs) = 0
jumlahSrimes n (x:xs) = srimes n (x:xs) + jumlahSrimes (n-1) (x:xs)

--25a rumus cepat 2 kali dibanding 25b

srimeee n = srimes n ((take (div n 7)) (iterate (7 +) (7)))


jumlahSrimeee 1  = 0
jumlahSrimeee n  = srimeee n + jumlahSrimeee (n-1)

--25b rumus lebih cepat 8 kali dibanding 25c
srimes' b (x:xs)
     | (length (uhuy 0 ( map (rem b) (x:xs))) == 3) && (length (uhuy 0 ( map (rem b) (delete 7 (x:xs)))) == 2) = 1
     | (length (uhuy 0 ( map (rem b) (x:xs))) == 4) && (length (uhuy 0 ( map (rem b) (delete 7 (x:xs)))) == 3) = 1
     | otherwise = 0

srime' b (x:xs)
       | rem b 7 /= 0 = 0
       | (length (uhuy 0 ( map (rem b) (x:xs))) == 3) && ( rem b 7 == 0) = 1
       | (length (uhuy 0 ( map (rem b) (x:xs))) == 4) && ( rem b 7 == 0) = 1
       | otherwise = 0


jumlahSrime' 1 (x:xs) = 0
jumlahSrime' n (x:xs) = srime' n (x:xs) + jumlahSrime' (n-1) (x:xs)

--25c (rumus lambat wahahahhaha)

jumlahSrimes' 1 (x:xs) = 0
jumlahSrimes' n (x:xs) = srimes' n (x:xs) + jumlahSrimes' (n-1) (x:xs)



--24
jumlahRem 0 m = 0
jumlahRem n m = rem n m + (jumlahRem (n-1) m)

jumlahRem' n m = (jumlahRem (rem n m) m) + ((div n m) * (jumlahRem 99 m))

--54?
--Terdiri dari 7 digits
--Digit ke 3 adalah 3
--Digit ke 6 adalah 5
--In short, the sum of all primes possessing this pattern **3**5*

-- dr angka 1.000.000 - 9.999.999
-- paling belakang tidak mungkin bisa dibagi 2

--39
turunan [a,b] = [(b*a), (b-1)]
turunanSatu [] = []
turunanSatu (x:xs) = turunan x : turunanSatu xs

turunanBanyak m (x:xs)
  | m == 1 = turunanSatu (x:xs)
  |otherwise = turunanBanyak (m-1) (turunanSatu (x:xs))

-- 31 106255


--map (\x -> x*x)
--[(x,y) | x<- [1..6], y <- [1..6]]

--[(x,y) | x<- [1..6], y <- [1..6], let c = x*y, odd c]

--[(a,b,c) | a <- [1..100], y<- [1..100], ]

--learnyouahaskell.com

--coba [x^2 | x <- [1..100]]

--coba [x | x <- [1..100]]

--sol1 lim = iter 1 0
--  where iter i resu

--divMod
--mod

--let y = div 100 10
--sqrt (fromIntegral y)
--round $ sqrt $ fromIntegral y
--ceiling $ sqrt $ fromIntegral y

--pitagoras = [(x,y,z)]











--batas cuy
--batas
--batasssss


--52--1981
--40 = 256353335039229952
--57 = 10092 = belum benar wakakkaka
--57 = 10689 = belum benar, knp?
--57 = 10321 = belum benar, knp?
--57 = 27175 = salah wakakkaka
--57???? 385?


pitgorr' a = help a (reverse [((div (a*7) 10))..(a-1)]) 0
  where help a [] res = res
        help x (y:ys) res
          | ((x^2) - (y^2)) == ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2) && ((x + y + ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2)) > 20000) = help x (delete (ceiling ( sqrt (fromIntegral ((x^2) - (y^2))))) (ys)) (res)
          | ((x^2) - (y^2)) == ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2) && ((x + y + ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2)) <= 20000) = help x (delete (ceiling ( sqrt (fromIntegral ((x^2) - (y^2))))) (ys)) (1+res)
          | ((x^2) - (y^2)) /= ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2) = help x ys res

ngitungPitgor 2 = 0
ngitungPitgor n = pitgorr' n + ngitungPitgor (n-1)

ngitungBener [] = 0
ngitungBener (x:xs)
  | sum x == 1000 = product x + ngitungBener xs
  | otherwise = ngitungBener xs


pitgorr a = help a (reverse [((div (a*5) 10))..(a-1)]) 0
  where help x [] res = res
        help x (y:ys) res
         | (((x^2) - (y^2)) == ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2)) = help x (delete (ceiling ( sqrt (fromIntegral ((x^2) - (y^2))))) (ys)) (1+res)
         | ((x^2) - (y^2)) /= ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2)  = help x ys res
         | otherwise = 0

ngitungPitgornya 2 = []
ngitungPitgornya n = pitgorrnya n ++ ngitungPitgornya (n-1)

pitgorrnya a = help a (reverse [((div (a*7) 10))..(a-1)])
  where help a [] = []
        help x (y:ys)
         | ((x^2) - (y^2)) == ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2)  = [[x,y,(ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))]] ++ (help x (delete (ceiling ( sqrt (fromIntegral ((x^2) - (y^2))))) (ys)))
         | ((x^2) - (y^2)) /= ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2) = help x ys


hapusPitgor [] = 0
hapusPitgor (x:xs)
  | (sum x) > 20000 = hapusPitgor xs
  | otherwise = 1 + hapusPitgor xs







--helpMe x y
--  | ((x^2) - (y^2)) == ((ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))))^2) = 1

--helpMePlease x y
--  | sqrt ((x^2) - (y^2)) == ceiling ( sqrt (fromIntegral ((x^2) - (y^2)))) = 1



pitgor''' a = [[x,y,z] |x<- [2..a], y<- [x..a], z<-[y..(x+y)], z^2 == x^2 + y^2]

pitgor' a = [[x,y,z] | z<- [5..a], x<- [(max 2 (div z 10))..z], y<- [(max x (div (z*8) 10))..z], x/= 0, z^2 == x^2 + y^2]


pitgor'' a = [[x,y,z] | z<- [5..a], x<- [2..z], y<- [x..z], z^2 == x^2 + y^2]


pitagoras a = [[x,y,z] | x <- [1..a], y <- [x..a], z <- [y..a], z^2 == x^2 + y^2]


--pitgor''' a = [[x,y,z] |x<- [2..a], y<- [x..z], z^2 == x^2 + y^2]
--pitgor'' a = [[x,y,z] | x<- [2..a], y<- [x..(9*x)], z<-[y..(2*y)], z^2 == x^2 + y^2]

pitgor a = [[x,y,z] | z<- [5..a], x<- [2..z], y<- [x..z], intersect (faktor x) (faktor y) == [] , z^2 == x^2 + y^2]



faktor n = faktorFaktorT n [2..ceiling $ sqrt $ fromIntegral n]
          where faktorFaktorT n [] = []
                faktorFaktorT n (x:xs)
                 | rem n x == 0 = [x] ++ [div n x] ++ faktorFaktorT n xs
                 | otherwise = faktorFaktorT n xs

--ngaliinpitgor n = ngali [1..]

--55

--primaKe n = (!!) (nyariPrimaT 1) (n-1)

primaKe n = cariPrimaKe 1 n

cariPrimaKe b 0 = b-1
cariPrimaKe b n = primaT b [2..ceiling $ sqrt $fromIntegral b] (n)
           where primaT b [] n = cariPrimaKe (b+1) (n-1)
                 primaT b (x:xs) n
                  | rem b x == 0 = cariPrimaKe (b+1) n
                  | otherwise = primaT b xs n






primaT' b = helpp b [2..ceiling $ sqrt $fromIntegral b]
  where helpp a [] = a
        helpp a (x:xs)
          | rem a x == 0 = 0
          | otherwise = helpp a xs


doublePrima [] = []
doublePrima (x:xs) = urutan (x-1) xs ++ doublePrima xs

hapus b (x:xs)
         | b == x = xs
         | otherwise = hapus b (xs)



--urut n (x:xs) = urutan ((head(urutan (n+1) (x:xs))) - (head(urutan n (x:xs)))) (hapus n (x:xs))

--urutann n x = urutan (x-n) (take 100 (nyariPrimaT 1))

--104729
urutan n [] = []
urutan n (x:xs)
    |n == (length [x]) = [x]
    |otherwise = urutan (n-1) (xs)

--primaKe' n = cariPrimaKe (primaKe (n-1))



--56 hasil = 148812
nyariPrimaT 100000000000000000000 = []
nyariPrimaT n =primaT' n [2..ceiling $ sqrt $fromIntegral n] ++ (nyariPrimaT (n+1))
   where primaT' n [] = [n]
         primaT' n (x:xs)
           | rem n x == 0 = []
           | otherwise = primaT' n xs





numToDigit' n
  | n < 2 = [n]
  | otherwise = numToDigit' (div n 2)  ++ [rem n 2]

ngitungNumToDigit (x:xs) = helper (x:xs) 0
   where helper [] result = result
         helper (x:xs) result = helper xs (length (numToDigit' x) + (result))


faktorT n = sort (faktor n)




--jawaban 123902842195004540663688712678113907 17700118743719262069688729975550286 (benar cuy)

--primaBenar [] = 0
--primaBenar (x:xs) = urutan' (x) (x:xs) + bantuin (x:xs)
--  where urutan' n [] = 0
--        urutan' n (x:xs)
--          | n == (length [x]) = x
--          | otherwise = urutan' (n-1) (xs)

--bantuin (x:xs)
--          | head xs == 104729 = (1366661)
--          | otherwise = (urutan' ((urutan' (head xs) (x:xs)) - (x)) (drop x (x:xs))) + bantuin xs
--            where urutan' n [] = 0
--                  urutan' n (x:xs)
--                   | n == (length [x]) = x
--                   | otherwise = urutan' (n-1) (xs)




--apakek [2,3,5,7,11] [2,3,5,7,11] = 3 + apakek [3,5,7,11] [5,7,11]
--urutanoy [104729] (x:xs) = 1366661
urutanoy [] _ = 0
urutanoy (n:ns) (x:xs) = urutan' n (x:xs) + urutanoy (ns) (x:xs)

urutan' n [] = 0
urutan' n (x:xs)
          | n == (length [x]) = x
          | otherwise = urutan' (n-1) (xs)

ambil a [] = []
ambil a (x:xs)
        | a == x = xs
        |otherwise = ambil a xs
fungsi x = (x^2) + x



