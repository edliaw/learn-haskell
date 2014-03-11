-- Informatics 1 - Functional Programming
-- Tutorial 1
--
-- Due: the tutorial of week 3 (4/5 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]


halveEvens' :: [Int] -> [Int]
halveEvens' = map (`div` 2) . filter even

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
  | x `mod` 2 == 0  = x `div` 2 : next
  | otherwise  = next
  where
    next = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens' xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

inRange' lo hi = filter (>= lo) . filter (<= hi)

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
  | lo <= x && x <= hi  = x : next
  | otherwise           = next
  where
    next = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange l h x = inRange l h x == inRangeRec l h x



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = sum [x | x <- xs, x > 0]

countPositives' :: [Int] -> Int
countPositives' = sum . filter (> 0)

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
  | x > 0      = x + rest
  | otherwise  = rest
  where
    rest = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives x = countPositives' x == countPositivesRec x



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round (0.9 * fromIntegral x)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs =
    sum [x | x <- map discount xs, x <= 19900 ]

pennypincher' :: [Int] -> Int
pennypincher' = sum . filter (<= 19900) . map discount 

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) 
  | discount x <= 19900  = discount x + rest
  | otherwise            = rest
  where
    rest = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher x = pennypincher x == pennypincherRec x



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits s = product [digitToInt c | c <- s, isDigit c]

multDigits' :: String -> Int
multDigits' = product . map digitToInt . filter isDigit

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (c:str)
  | isDigit c  = digitToInt c * rest
  | otherwise  = rest
  where
    rest = multDigitsRec str

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits s = multDigits s == multDigitsRec s



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise s = toUpper (head s) : map toLower (tail s)

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (c:str) = toUpper c : lcRest str
  where
    lcRest :: String -> String
    lcRest [] = []
    lcRest (c:str) = toLower c : lcRest str

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise s = capitalise s == capitaliseRec s



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title ss = [if length s >= 4 then capitalise s else s | s <- ss]

title' :: [String] -> [String]
title' = map (\s -> if length s >= 4 then capitalise s else s)

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (s:ss)
  | length s >= 4  = capitalise s : next
  | otherwise      = s : next
  where
    next = titleRec ss


-- mutual test
prop_title :: [String] -> Bool
prop_title s = title s == titleRec s




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind c pos len words
  = [w | w <- words,
     0 <= pos, pos < len,
     length w == len,
     w !! pos == c]

crosswordFind' :: Char -> Int -> Int -> [String] -> [String]
crosswordFind' c inPos len 
  | 0 <= pos && pos < len 
  = filter ((== c) . (!! inPos)) . filter ((== len) . length)
  | otherwise  = error "Position must be in length"

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec = undefined

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind = undefined



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search = undefined

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined
