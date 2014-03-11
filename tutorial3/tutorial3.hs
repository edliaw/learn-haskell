-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 17/18 Oct.

import Data.Char
import Data.List (transpose)
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper

-- b.
doubles :: [Int] -> [Int]
doubles = map (* 2)

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x -> (fromIntegral x) / 100)

-- d.
uppers' :: String -> String
uppers' str = [toUpper c | c <- str]

prop_uppers :: String -> Bool
prop_uppers str = uppers' str == uppers str



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar ch = filter (/= ch)

-- c.
above :: Int -> [Int] -> [Int]
above lim = filter (> lim)

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (uncurry (/=))

-- e.
rmCharComp :: Char -> String -> String
rmCharComp ch str = [c | c <- str, c /= ch]

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch str = rmCharComp ch str == rmChar ch str



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = map toUpper . filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (* 2) . filter (> 3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even $ length s]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (xs:xss) = xs ++ concatRec xss

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec _ []  = []
rmCharsRec [] str = str
rmCharsRec (ch:rm) str = rmCharsRec rm $ rmChar ch str

rmCharsFold :: String -> String -> String
rmCharsFold rm str = foldr rmChar str rm

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix a = [[a]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (== x) xs

-- b.
valid :: Matrix a -> Bool
valid [] = False
valid m = hasCol m && (uniform $ map length m)
  where hasCol = not . null . head

-- 6.

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x, y) <- zip xs ys]

zipWithMap :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithMap f xs ys = map (uncurry f) (zip xs ys)

heightM :: Matrix a -> Int
heightM = length

widthM :: Matrix a -> Int
widthM = length . head

-- 7.
plusM :: Num a => Matrix a -> Matrix a -> Matrix a
plusM m1 m2 | ok = zipWith plusRow m1 m2
  where
    ok = valid m1 && valid m2
         && heightM m1 == heightM m2
         && widthM m1 == widthM m2
    plusRow = zipWith (+)

-- 8.
timesM :: Num a => Matrix a -> Matrix a -> Matrix a
timesM m1 m2 | ok = [[dot r1 c2 | c2 <- transpose m2] | r1 <- m1]
  where
    ok = valid m1 && valid m2
         && heightM m1 == widthM m2
    dot :: Num a => [a] -> [a] -> a
    dot v1 v2 = sum $ zipWith (*) v1 v2

-- Optional material
-- 9.

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix f = map (map f)

zipMatrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrix f = zipWith (zipWith f)

signM :: Num a => Int -> Int -> Matrix a
signM w h = cycleN h [evenRow, oddRow]
  where
    evenRow = cycleN w [1, -1]
    oddRow = cycleN w [-1, 1]
    cycleN :: Int -> [a] -> [a]
    cycleN n x = take n $ cycle x

signM' :: Num a => Int -> Int -> Matrix a
signM' w h = [[(-1) ^ (i + j) | i <- [1..w]] | j <- [1..h]]

removes :: [a] -> [[a]]
removes [] = []
removes (x:xs) = xs : next
  where next = map (x :) (removes xs)

minors :: Matrix a -> Matrix (Matrix a)
minors m = undefined

cofactors :: Matrix a -> Matrix a
cofactors m = undefined

determinant :: Matrix a -> a
determinant = undefined

scaleM :: Num a => a -> Matrix a -> Matrix a
scaleM k = mapMatrix ( * k)

inverseM :: Fractional a => Matrix a -> Matrix a
inverseM m = scaleM (1 / determinant m) (transpose $ cofactors m)

prop_inverse :: Eq a => Fractional a => Matrix a -> Bool
prop_inverse m = m == (inverseM . inverseM) m
