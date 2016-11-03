File pa1.lhs for Practical 1: Factoring Numbers

Finding the smallest prime factor

> factor :: Integer -> (Integer,Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer,Integer)
> factorFrom m n
>   | r == 0      = (m,q)
>   | n <= m*m    = (n,1)
>   | otherwise = factorFrom (m+1) n
>   where (q,r) = divMod n m

Exercise 1: 
* Since n = sqrt(n)*sqrt(n), for any two numbers (a,b) to be factors of n such that n=a*b, one of a or b has to be smaller than or equal to sqrt(n) with the other being larger, or equal. We can stop searching for factors after crossing the integer sqrt(n), if it exists, or alternatively the factor m where m*m >= n since effectively we will have searched the space of numbers larger than m, by having searched the space of numbers smaller than m.

*No they cannot be interchanged. It would not work for the case where m=q=sqrt(n).

*Approximately sqrt(n) calls are needed in the worst case.

Exercise 2: factor 0 = (2,0); factor 1 = (1,1)

Exercise 3:
*Main> factor 1
(1,1)
*Main> factor 0
(2,0)

An improved version, as explained in the text of the practical:

> factorFrom1 :: Integer -> Integer -> (Integer,Integer)
> factorFrom1 m n
>   | r == 0     = (m,q)
>   | q <= m     = (n,1)
>   | otherwise  = factorFrom1 (m+1) n
>   where (q,r) = divMod n m

Exercise 4:
n = q*m (+ r if it is not the factor), then q*m <= m*m is the same as n <= m*m. It is more efficient since we don't have to compute m*m in each iteration, rather we compare it with m only.

Exercise 5:

> factor2 :: Integer -> (Integer,Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 ::  Integer -> Integer -> (Integer,Integer)
> factorFrom2 m n
>  | r == 0	= (m,q)
>  | q <= m	= (n,1)
>  | m == 2	= factorFrom2 (m+1) n
>  | otherwise  = factorFrom2 (m+2) n
>  where (q,r) = divMod n m	

I would expect it to be around 50% faster, since effectively we are removing half of the possible candidates to be checked.

Exercise 6:
*Main> factor2 22
(2,11)
*Main> factor2 54
(2,27)
*Main> factor2 107861234213
(107861234213,1)

[PS: in the third example, I did not pick a random number and try it, it was purely by chance that I typed it out! A for effort I think.]

Exercise 7:

> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 2 n 2

> factorFrom3 :: Integer -> Integer -> Integer -> (Integer,Integer)
> factorFrom3 m n s
>  | r == 0	= (m,q)
>  | q <= m	= (n,1)
>  | m == 2	= factorFrom3 (m+1) n 2
>  | m == 3	= factorFrom3 (m+2) n 2
>  | otherwise  = factorFrom3 (m+s) n (6-s)
>  where (q,r) = divMod n m

*Main> 3*41
123
*Main> factor3 107861234213
(107861234213,1)
*Main> factor3 21
(3,7)
*Main> factor3 25
(5,5)

Finding all prime factors

> primeFactors :: Integer -> [Integer]
> primeFactors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n =
>    if n == 1 then [] else p:factorsFrom p q
>    where (p,q) = factorFrom m n

Exercise 8:
The downside to using only prime numbers as trial divisors, is that we now have to store the prime factors as we go, and for large numbers this entails large memory requirements.

Exercise 9:

> primeFactors2 :: Integer -> [Integer]
> primeFactors2 n = factorsFrom 2 n

> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n =
>    if n == 1 then [] else p:factorsFrom2 p q
>    where (p,q) = factorFrom3 m n 2

Exercise 10:
*Main> primeFactors2 768351234214
[2,384175617107]
(0.73 secs, 350960672 bytes)
*Main> primeFactors 768351234214
[2,384175617107]
(0.74 secs, 349449176 bytes)

*Main> primeFactors2 7683455478653376245
[5,102001,15065451277249]
(4.48 secs, 2197431416 bytes)
*Main> primeFactors 7683455478653376245
[5,102001,15065451277249]
(4.51 secs, 2210571936 bytes)



This completes the script.
