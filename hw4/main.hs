prime 1 = False
--prime n = null [x | x<- [2.. (n-1)], n `mod` x == 0]
prime n = null [x | x<- [2.. sqn], n `mod` x == 0]
				where sqn = (floor(sqrt(fromIntegral n)))

primes = filter prime [1..]

primeSqSum start end = [x^2 | x<-(filter prime[start..end])]


-- seriesSum n = take n [1, -1/2, 1/3..]