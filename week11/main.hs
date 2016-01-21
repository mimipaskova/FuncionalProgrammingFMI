
-- 1 zad
saySign x
    | x > 0 = "Positive"
    | x == 0 = "Zero"
    | otherwise = "Negative"
    

    
-- 2 zad
useless 0 _ _ _ = 1
useless _ 0 _ _ = 1
useless _ _ 0 _ = 1
useless _ _ _ 0 = 1
useless a b c d = a + b + c + d

-- 3 zad
maxThree a b c
    | a < b = if b < c then c else b
    | otherwise = if a > c then a else c
    
-- 4 zad    
fact1 0 = 0
fact1 1 = 1
fact1 n = n*fact1(n-1)

fact2 x
    | x < 2 = 1
    | otherwise = x * fact2 (x-1)   
    
    
fact3 x = if x < 2 then 1 else x*fact3(x-1)

fact4 x = case x of 0 -> 1
                    x -> x * fact4 (x-1)

--test = fact4 5 ==120

hypot a b = let square x = x * x
                a2 = square a
                b2 = square b
                in sqrt (a2 + b2)

hypot2 a b
        | a < 0 || b < 0 = 0
        | otherwise = sqrt (a2 + b2)
            where square x = x * x
                  a2 = square a
                  b2 = square b                

-- 6 zad
countRoots a b c
        | d < 0 = "No roots"
        | d == 0 = "One root"
        | otherwise = "Two roots"
        where d = b * b - 4 * a * c

discr a b c = b * b - 4 * a * c

-- 7 zad
sayRoots a b c
    | discr a b c < 0 = "No roots"
    | prod < 0 = "Positive and negative"
    | prod == 0 = "One of them is a zero"
    | sum > 0 = "Both positive"
    | otherwise = "Both negative"
    where sum = (-b)/a
          prod = c/a


-- 5 zad
fibonacci n = fibhelp 0 1 1
        where fibhelp a b i
                | i == n = b
                | otherwise = fibhelp b (a+b) (succ i)

-- 8 zad
cylinderVolume 0 0 = 0
cylinderVolume 0 _ = 0
cylinderVolume r 0 = 0
cylinderVolume r h = r^2*h*pi