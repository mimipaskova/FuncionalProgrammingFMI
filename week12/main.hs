umn x y = x * y

--1 zad
replicatee x y 
	| x == 0    = []
	| otherwise	= y : replicatee (x-1) y


replicateee n x = take n (repeat x)

--2 zad
takee n arr
	| null arr = []
	| n == 0 = []
	| otherwise = head arr : (takee (n-1) (tail arr))


--7 zad
descartes l1 l2 = [ (x,y) | x<-l1, y<-l2]

divisors n = [x | x <-[1..n], n `mod` x == 0]

--prime n = length (divisors n) > 2

--3 zad
prime 1 = False
--prime n = null [x | x<- [2.. (n-1)], n `mod` x == 0]
prime n = null [x | x<- [2.. sqn], n `mod` x == 0]
				where sqn = (floor(sqrt(fromIntegral n)))

--4 zad
primes = filter prime [1..]				

--5 zad
nthPrime n = primes !! (n-1)

--8 zad
pyths = [(a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], a^2 + b^2 == c^2, a + b + c <100]


--(a,b,c)
first  (a,_,_) = a
second (_,b,_) = b
third  (_,_,c) = c