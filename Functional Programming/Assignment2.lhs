> {-# LANGUAGE NPlusKPatterns #-}

Question 1.

a) Foldl (-) x xs = x - sum xs 
   TRUE
		foldl (-) x xs = (x- x1) - x2) - x3).. - xn)
					   = x - x1 - x2 - x3 .. - xn
					   = x - (x1 + x2 + x3 ... + xn)
					   = x - sum(xs)

b) Foldr (-) x xs = x - sum xs
   FALSE
   		foldr (-) x xs = (x1 - (x2 - (x3 - .. (xn - x))))
   					   = x1 - x2 + x3 .. xn
   					  != x1 - sum xs



Question 2.

> placeCorrectly :: Ord a => [a] -> a -> [a]
> placeCorrectly [] x = [x]
> placeCorrectly xs x = if x < head xs  then x:xs else (head xs):(placeCorrectly (tail xs) x)

> isort :: Ord a => [a] -> [a]
> isort = foldl placeCorrectly [] 

*Main> isort [5,4,3,2,1]
[1,2,3,4,5]


Question 3.

> remdups :: Eq a => [a] -> [a]
> remdups = foldr buildL []

> buildL :: Eq a => a -> [a] -> [a]
> buildL x [] = [x]
> buildL x xs = if x == (head xs) then xs else (x:xs)

*Main> remdups [1,2,2,3,3,3,1,1]
[1,2,3,1]

Question 4.

scanl1 (+) (scanl (/) 1 [1..n])
{definition of scanl}
scanl1 (+) 1 : scanl (/) (/ 1 1) [2..n]
{which evaluates to}
scanl1 (+) [1,1,1/2, 1/2/3, ...]
{definition of scanl1}
[1, 1+1, 1+2+1/2, 1+2+1/2+1/2/3, ...]

Question 5.

> type Matrix a = [[a]]
> type IntMat = Matrix Integer

a) 

> scale :: Integer -> IntMat -> IntMat
> scale s m = map (map (s * )) m

*Main> x = [[1,2,3] , [5,6,7]]
*Main> scale 3 x
[[3,6,9],[15,18,21]]

b)

> addMat :: IntMat -> IntMat -> IntMat
> addMat [] [] = []
> addMat (x:xs) (y:ys) = zipWith (+) x y : addMat xs ys

*Main> addMat x x
[[2,4,6],[2,4,6]]

c)

> transpose :: Matrix a -> Matrix a
> transpose ([]:_) = []
> transpose (_:[]) = []
> transpose x = (map head x) : transpose (map tail x)

*Main> transpose x
[[1,1],[2,2],[3,3]]

d)

> dotp :: [Integer] -> [Integer] -> Integer
> dotp xs ys = sum (zipWith (*) xs ys)

> multMat :: IntMat -> IntMat -> IntMat
> multMat m1 m2 = [[dotp a b | b <- transpose(m2)] | a <- m1]

e)

first find the size of each column:

> columnWidths x = [[ (length .show) b| b <- a]| a <- transpose(x)]
> widestColumns x = map maximum (columnWidths x)

> rjustify :: (Int,String) -> String
> rjustify (w,st) = spaces (w-length st) ++ st

> spaces :: Int -> String
> spaces n = replicate (n+1) ' '

> showRow ws row =
> 		concat [rjustify (w, show k) |
> 				(w,k) <- zip ws row] ++ "\n"

> showMat table = concat [[ showRow (widestColumns table) rowT | rowT <- table ]]

*Main> table = [[1,-500,-4], [100,15043,6], [5,3,10]]
*Main> putStr (concat (showMat table))
   1  -500 -4
 100 15043  6
   5     3 10

Question 7.
a)

> cube a = a*a*a
> taxicab n = [ (cube a + cube b, (a,b), (c,d)) |
>		      a <- [1..n],
>			  b <- [a+1..n],
>			  c <- [a+1..n],
>			  d <- [c+1..n],
>			  (cube a + cube b) == (cube c + cube d)]

b)

*Main> taxicab 20
[(1729,(1,12),(9,10)),(4104,(2,16),(9,15))]

We can see that the second taxicab number is 4104

c) Yes, there are infinitely many of the taxicab numbers.

Question 8.

a)

[(x,y) | x <- [1..n], x odd, y <- [1..n]]

{using LC7: }
concat [ [(x,y)|x odd, y<-[1..n]] | x<-[1..n] ]

{using Law 1}
concat [ if x odd then [(x,y)| y<- [1..n]] else []| x<-[1..n]]

{define f x = if x odd then map (\y -> (x,y))[1..n] else []}
concat [ f x | x <-[1..n] ]

{using LC5}
concat ( map f [1..n]) where f x = 
							if x odd then map (\y -> (x,y))[1..n] else []

b)

[ (x,y) | x <- [1..n], y <- [1..n], odd x]

{using LC3}
concat (map f [1..n]) where f x = [(x,y)| y <- [1..n], odd x]

	{claim f [e|Q,p] where Q = y <- ys and p = f x, where y and x are independent = f [ e| p ,Q]}
	f x = [(x,y)| odd x, y <- [1..n])

	{using LC4}
	f x = if odd x then ((x,y)| y<- [1..n]) else []

	{using LC1}
	f x = if odd x then (map (\y -> (x,y)) [1..n]) else []

{finally:}
concat ( map f [1..n]) where f x =
								if odd x then (map (\y -> (x,y)) [1..n]) else []

proof of claim:
consider the list comprehension [{x,y}| y<-[1..n], odd x]

{using LC3}
= concat (map f [1..n]) where f x = [(x,y)|odd x]

{using LC2}
= concat (map f [1..n]) where f x = if odd x then [(x,y)] else []

{using LC1}
= concat (if odd x then map (\y -> (x,y)) [1.n] else [])

{using LC1}
= concat (if odd x then [ (x,y) |y <- [1..n]] else []

{using LC4}
= concat ( [(x,y)| odd x, y <- [1..n]])

{using LC7}
= [(x,y)| odd x, y<-[1..n]]

c) Yes, they are equal, as we can see above

3) Yes, the first one is more likely to be efficient since it applies the condition on odd x first, and then forms the combinations (x,y) through list comprehension, whereas the second one forms (x,y) first, and then maps odd x onto it, resulting in iterating through a far larger list
