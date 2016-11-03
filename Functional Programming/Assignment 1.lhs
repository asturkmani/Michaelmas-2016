> {-# LANGUAGE NPlusKPatterns #-}

Question 1. Define (&&) and (||) using conditionals (i.e. if statements).


> import Data.Char
> myAnd :: Bool -> Bool -> Bool
> myAnd a b
>   | a == False = False
>   | b == False = False
>   | otherwise = True

Example Output:
*Main> myAnd True False
False
*Main> myAnd True True
True

> myOr :: Bool -> Bool -> Bool
> myOr a b
>   | a == True = True
>   | b == True = True
>   | otherwise = False

Example Output:
*Main> myOr True False
True
*Main> myOr False False
False

Question 2.
(a) Define a function nextlet that takes a capital letter of the alphabet and returns 
the letter coming immediately after it. Assume that ’Z’ is followed by ’A’.

> nextlet :: Char -> Char
> nextlet a = 
>   if ord a < 90 then chr (ord a + 1)
>   else 'A'

Example Output:
*Main> nextlet 'D'
'E'
*Main> nextlet 'Z'
'A'
*Main> nextlet 'A'
'B'


(b) Define a function digitval that converts a digit character to its corresponding 
numerical value.

> digitVal :: Char -> Int
> digitVal a = ord a - 48

Example Output:
*Main> digitVal '9'
9

Question 3. Suppose a date is represented by a triple (d, m, y) of three integers, where d
is the day, m is the month, and y is the year. Define a function age that takes two dates,
the first being the current date, and the second being a person’s birth date, and returns
the age of that person. Include a suitable type signature for your function, using a suitable
type synonym.

> type Day = Int
> type Month = Int
> type Year = Int
> age :: (Day, Month, Year) -> (Day,Month,Year) -> Int
> age (d1,m1,y1) (d2,m2,y2)
>   | m2 < m1  || (m2 == m1 && d2 < d1) = y2-y1-1
>   | otherwise = y2-y1

Example Output:
*Main> age (21,7,1994) (21,7,2016)
22
*Main> age (21,7,1994) (20,7,2016)
21
*Main> age (21,7,1994) (20,6,2016)
21
*Main> age (21,7,1994) (20,8,2016)
22


Question 4. Write a function sum :: [Integer] -> Integer that returns the sum of
the elements in a list.

> mySum :: [Integer] -> Integer
> mySum [] = 0
> mySum [x] = x
> mySum (x:xs) = x + sum xs

Example Output:
*Main> mySum [1,2,3,4,5,6,7,8,9,10]
55

Question 5. Write a function maximum :: [Integer] -> Integer that returns the
maximum element in a list.

> myMaximum :: [Integer] -> Integer
> myMaximum [x] = x
> myMaximum (x:xs) = max x (maximum xs)

Example Output:
*Main> myMaximum [-1,-5,5,0,3]
5
*Main> myMaximum [-1,-5,-3,-2]
-1

Question 6. Some of the equations below cannot possibly hold because they are not
correctly-typed; you should identify those equations for which this is the case. For those
equations that are correctly typed, what can you say about the type of xs and whether
the equation holds?

(a) []:xs = xs              
Correctly typed. xs :: [[a]]. Equation holds.
(b) []:xs = [[],xs]    
Correctly typed. Equation doesn't hold.   
(c) xs:[] = xs
Correctly typed. Equation doesn't hold.
(d) xs:[] = [xs]
Correctly typed. xs :: [a] or xs :: a or actually be of any type. Equation holds.
(e) xs:xs = [xs,xs]
Incorrectly typed
(f) [[]] ++ xs = xs
Correctly typed. Equation doesn't hold
(g) [[]] ++ xs = [xs]
Correctly typed. Equation doesn't hold
(h) [[]] ++ xs = [[],xs]
Correctly typed. xs :: [[a]] Equation holds.
(i) [[]] ++ [xs] = [[],xs]
Correctly typed. xs :: [a]. Equation holds.
(j) [xs] ++ [] = [xs]
Correctly typed. Equation doesn't hold. 
(k) [xs] ++ [xs] = [xs,xs]
Correctly typed. xs :: [a]. Equation holds. 

Question 7. Write a function to convert integers (less than 5000, say) into Roman
numerals. If you don’t know about Roman numerals, then try searching the Web.

> orderNumerals :: Char -> Char -> Char -> Integer -> String
> orderNumerals a b c i = 
>  [[a],[a,a],[a,a,a],[a,b],[b],[b,a],[b,a,a],[b,a,a,a],[a,c]] !! (fromIntegral i - 1)

> toRoman :: Integer -> String
> toRoman 0 = ""
> toRoman x | x < 0     = error "Cannot handle negative numbers"
> toRoman x | x >= 1000 = replicate (fromIntegral q) 'M' ++ toRoman r where
>   (q,r) = x `divMod` 1000
> toRoman x | x >= 100  = orderNumerals 'C' 'D' 'M' q ++ toRoman r where 
>   (q,r) = x `divMod` 100
> toRoman x | x >= 10   = orderNumerals 'X' 'L' 'C' q ++ toRoman r where 
>   (q,r) = x `divMod` 10
> toRoman x             = orderNumerals 'I' 'V' 'X' x

Example Output:
*Main> toRoman 199
"CXCIX"
*Main> toRoman 4919
"MMMMCMXIX"

Question 8. Time for a song:

> numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

> song :: Int -> String
> line1 :: Int -> String
> line3 :: Int -> String
> sentence :: Int -> String
> loopSentence :: Int -> String

> song 0 = ""
> song (n+1) = song n ++ "\n" ++ verse (n+1)
> verse n = line1 n ++ line ++ line3 n ++ line

> line = "Went to mow a meadow \n"
> line1 1 = capitalize(sentence 1) ++ " went to mow \n"
> line1 (n+2) = capitalize(init (sentence (n+2))) ++ " went to mow \n"
> line3 (n+1) = capitalize(loopSentence (n+1) ++ "and his dog \n")

> loopSentence 0 = ""
> loopSentence (n+1) = sentence (n+1) ++ " " ++ loopSentence n
> sentence 0 = ""
> sentence (n+1)
>   | (n+1) == 1 = (numbers !! n) ++ " man"
>   | otherwise = (numbers !! n) ++ " men,"

> capitalize (x:xs) = [toUpper(x)] ++ xs

Example Ouput:

One man went to mow 
Went to mow a meadow 
One man and his dog 
Went to mow a meadow 

Two men went to mow 
Went to mow a meadow 
Two men, one man and his dog 
Went to mow a meadow 

Three men went to mow 
Went to mow a meadow 
Three men, two men, one man and his dog 
Went to mow a meadow 

Four men went to mow 
Went to mow a meadow 
Four men, three men, two men, one man and his dog 
Went to mow a meadow 
