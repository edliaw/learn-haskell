-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 10/11 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate i str
  | i < 0           = error "Can't rotate by negative amount"
  | i > length str  = error "Must rotate by an amount less than the length of the string"
  | otherwise       = drop i str ++ take i str

rotate' :: Int -> [Char] -> [Char]
rotate' i str = drop m str ++ take m str
  where l = length str
        m = if l == 0 then 0 else i `mod` l

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
  where l = length str
        m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey i = zip alphabet (rotate i alphabet)
  where alphabet = ['A'..'Z']

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp q [] = q
lookUp q ((k,v):keys)
  | q == k     = v
  | otherwise  = lookUp q keys

-- 5.
encipher :: Int -> Char -> Char
encipher i c = lookUp c (makeKey i)

-- 6.
normalize :: String -> String
normalize str = [toUpper c | c <- str, isAlphaNum c]

-- 7.
encipherStr :: Int -> String -> String
encipherStr i str = [encipher i c | c <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey keys = [(v, k) | (k, v) <- keys]

-- 9.
decipher :: Int -> Char -> Char
decipher i c = lookUp c (reverseKey $ makeKey i)

decipherStr :: Int -> String -> String
decipherStr i str
  = [decipher i c
    | c <- str,
      isUpper c
      || isDigit c
      || c == ' ']

-- 10.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str sub = sub `isPrefixOf` str || contains (tail str) sub

-- 11.
candidates :: String -> [(Int, String)]
candidates str
  = [(i, candidate) | i <- [0..26],
     let candidate = decipherStr i str,
     candidate `contains` "THE"
     || candidate `contains` "AND"]


-- Optional Material

-- 12.

split :: Int -> String -> [String]
split _ [] = []
split i str
  | i <= 0    = error "0 or negative chunk size"
  | i > l     = [str ++ fillX]
  | otherwise = take i str : next
  where l = length str
        fillX = replicate (i - l) 'X'
        next = split i $ drop i str

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive str = split 5 str

-- 13.
prop_transpose :: String -> Bool
prop_transpose str = splitStr == (transpose $ transpose splitStr)
  where splitStr = splitEachFive str

-- 14.
encrypt :: Int -> String -> String
encrypt i str = concat . transpose . splitEachFive $ encipherStr i str

-- 15.

splitTransposed :: String -> [String]
splitTransposed str
  | l `mod` 5 == 0  = split i str
  where l = length str
        i = l `div` 5

decrypt :: Int -> String -> String
decrypt i str = concat . transpose . splitTransposed $ decipherStr i str

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs [] = []
countFreqs (q:str) = (q, freq) : next
  where freq = 1 + sum [1 | c <- str, c == q]
        next = countFreqs [c | c <- str, c /= q]

-- 17
freqDecipher :: String -> [String]
freqDecipher str = [decrypt (distFromE c) str | (c, _) <- sortedFreqs]
  where distFromE c        = (ord c - ord 'E') `mod` 26
        sortedFreqs        = sortBy cmpSnd (countFreqs str)
        cmpSnd (_,x) (_,y) = compare y x
