--1 zad
square :: Num a => a -> a
square x = x * x

multiply :: Num a => a -> a -> a -> a
multiply x y z = x*y*z

--2 zad
inc :: Num a => a -> a
inc x = x + 1

dec :: Num a=> a -> a
dec x = x - 1

--3 zad
even' :: Integral a => a -> Bool
even' x = x `mod` 2 == 0

odd' :: Integral a => a -> Bool
odd' x = x `mod` 2 /= 0

-- 4 zad
between :: (Num a, Ord a) => a -> a -> a -> Bool
between a b c = a > b && a < c 

--5 zad
pyth :: (Num a, Eq a) => a -> a-> a-> Bool
pyth a b c = a^2 +b^2 == c^2

--6 zad - pattern matching
lucky :: (Num a, Eq a) => a -> String
lucky 7 = "Correct!"
lucky x = "Try again!"
--lucky _ = "Try again!"

--7 zad
factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

--8 zad
fibonacci :: (Num a, Eq a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)
