-- 1 zad
prime 1 = False
prime n = null [x | x<- [2.. sqn], n `mod` x == 0]
				where sqn = (floor(sqrt(fromIntegral n)))

primes = filter prime [1..]	

primeSqSum start end =sum [x^2 | x<-(filter prime[start..end])]

-- 2 zad
getNumber n = if (n `mod ` 2 == 0 ) then  (-1/(fromIntegral n)) else (1/(fromIntegral n))
seriesSum n =  sum $ map getNumber (take n [1..])


-- 3 zad
slice 1 end lst = take end lst
slice start end (h:t) = slice (start - 1) (end-1) t

-- 4 zad
removeNths n [] = []
removeNths n lst = (take (n-1) lst) : removeNths n (drop (n) lst)

removeNth n lst = foldr (++) [] $ removeNths n lst

-- 5 zad
merge [] lst2 = lst2
merge lst1 [] = lst1
merge (h1:t1) (h2:t2) = if (h1 < h2) then (h1 : (merge t1 (h2:t2) )) else (h2 : merge (h1:t1) t2)

-- 6 zad
mergeSort [] = []
mergeSort [h] = [h]
mergeSort (h:t) = merge [h] (mergeSort t)

-- 7 zad
goldbach n = head [(a,b) | let ps = takeWhile (<n) primes, a<-ps, b<-ps, a+b==n]