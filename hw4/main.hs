-- 1 zad
prime 1 = False
prime n = null [x | x<- [2.. sqn], n `mod` x == 0]
				where sqn = (floor(sqrt(fromIntegral n)))

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


mergeSort [] lst2 = lst2
mergeSort lst1 [] = lst1
mergeSort (h1:t1) (h2:t2) pred = if (h1 `pred` h2) then (h1 : (mergeSort t1 (h2:t2) pred )) else (h2 : mergeSort (h1:t1) t2 pred)

mergeSort [] lst2 = lst2
mergeSort lst1 [] = lst1
mergeSort (h1:t1) (h2:t2) = if (h1 > h2) then (h1 : (mergeSort t1 (h2:t2) )) else (h2 : mergeSort (h1:t1) t2)










-- removeNth 1 (h:t) = t
-- removeNth n (h:t) = h : removeNth (n-1) t

-- getNths n end= [n, n+n..end]
-- -- getNths n) (removeNth n lst)
-- -- remove n lst = [x | y<-(getNths n (length lst)), x<-(removeNth y lst)]

-- -- tova gurmi

-- remove n lst = map  (\x -> removeNth x lst) (getNths n (length lst))

-- -- remove n lst = map  product [n, n+n..]

-- -- tova e vzeto ot drugade i gurmi
-- sumProduct ls = sum $ map product ls



