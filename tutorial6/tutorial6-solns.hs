-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Solutions

-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import System.Random

-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen =  maximum . map (length . fst . snd)

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (b,(i,u)) = b ++ "..." ++ i ++ replicate (n + 3 - length i) '.' ++ u

showCatalogue :: Catalogue -> String
showCatalogue c = unlines (map (formatLine (longestProductLen xs)) xs)
    where
      xs = toList c

-- Note that the library function 'unlines' can be replaced by:
-- 
--   foldr (\xs ys -> xs ++ "\n" ++ ys) ""

-- Exercise 2

-- The values it returns (for different barcodes) are
--   Just ("The Macannihav'nmor Highland Single Malt", "75ml bottle")
--   Just ("Bagpipes of Glory", "6-CD Box")
--   Just ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")
--   Just ("Universal deep-frying pan", "pc")
--   Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs cat = catMaybes $ map (flip get cat) xs

-- Exercise 4
--
-- 'makeDB' takes approximately 2 seconds
--
-- 'get' takes between 0.00 and 0.06 seconds
--
-- a database of twice the size would have twice the lookup time
--
-- 'get' sees all (other) keys (104650) before it finds the last one


-- Exercise 11
--
-- 'makeDB' takes approximately 15 seconds
--
-- 'get' takes less than 0.01 seconds
--
-- 'get' sees at most as many keys as the tree is deep (40)




-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)