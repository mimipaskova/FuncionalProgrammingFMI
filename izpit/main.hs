import Data.List

-- Primerni zad kontrolno 2, 
type Point = (Double, Double)

dist:: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

maxDistance lst = maximum [dist x y | x<-lst, y<-lst]

-- kontrolno 2 zad 3
type Item = (String, Integer)

cmp (n1,ttl1) (n2, ttl2) = compare ttl1 ttl2
expiringItems :: [Item] -> (String, Int, String)
expiringItems lst = (fst $ head (sortBy cmp ([(name,ttl) | (name,ttl)<-lst, ttl>=0])),
	length( [name | (name, ttl)<- lst, ttl<0]),
	[head $ name |(name,ttl)<-lst, let mini = minimum [b | (a,b)<-lst], ttl == mini])

-- podgotovka za izpit zad 4
type Battery = (Int, Double)
bestBattery :: [Battery] -> Int -> Double
bestBattery lst number = minimum [price | (cap, price)<-lst, cap>=number]