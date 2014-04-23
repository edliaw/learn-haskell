-- Informatics 1 - Functional Programming
-- Tutorial 4
--
-- Due: the tutorial of week 6 (24/25 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:c.banks@ed.ac.uk\">Chris Banks</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:c.banks@ed.ac.uk\">Chris Banks</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Chris Banks","c.banks@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString a b = map toUpper a == map toUpper b


-- 2.
prefix :: String -> String -> Bool
prefix substr str = substr `sameString` take (length substr) str

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains (c:cs) substr = prefix substr (c:cs) || contains cs substr

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = (map toLower str) `contains` substr &&
                        (map toUpper str) `contains` substr
  where substr = take n (drop m str)


-- 4.
takeUntil :: String -> String -> String
takeUntil _ [] = ""
takeUntil substr (c:cs)
  | prefix substr (c:cs) = ""
  | otherwise = c : takeUntil substr cs

dropUntil :: String -> String -> String
dropUntil _ [] = ""
dropUntil substr str
  | prefix substr str = drop (length substr) str
  | otherwise = dropUntil substr $ tail str


-- 5.
split :: String -> String -> [String]
split "" str = [str]
split sep str
  | str `contains` sep = takeUntil sep str : split sep (dropUntil sep str)
  | otherwise = [str]

reconstruct :: String -> [String] -> String
reconstruct _ [] = ""
reconstruct sep ss = foldr1 f ss
  where
    f xs ys = xs ++ sep ++ ys

reconstruct' :: String -> [String] -> String
reconstruct' _ [] = ""
reconstruct' _ [s] = s
reconstruct' sep (s:ss) = s ++ sep ++ reconstruct' sep ss

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = tail $ split "<a href=\"" html

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails = filter (`contains` "mailto:")


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link
  | link `contains` "mailto" = (name, email)
  | otherwise = error "Link must contain a mailto: address"
  where name = dropUntil "\">" $ takeUntil "</a>" link
        email = dropUntil "mailto:" $ takeUntil "\">" link


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail substr emails = [(n, e) | (n, e) <- emails, n `contains` substr]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name $ emailsFromHTML html


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
