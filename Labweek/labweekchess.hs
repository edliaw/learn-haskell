-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 = p `above` invert p
  where p = twoBeside knight

pic2 :: Picture
pic2 = p `above` flipV p
  where p = twoBeside knight


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (whiteSquare `beside` blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

emptyRows :: Picture
emptyRows = emptyRow `above` otherEmptyRow

middleBoard :: Picture
middleBoard = repeatV 2 (emptyRows)

-- d)

pieces :: Picture
pieces = foldl (beside) rook [knight, bishop, queen, king, bishop, knight, rook]

-- e)

populatedBoard :: Picture
populatedBoard = blackPieces `above` middleBoard `above` whitePieces
  where whitePawns = repeatH 8 pawn
        blackPawns = invert whitePawns
        whitePieces = (whitePawns `above` pieces) `over` emptyRows
        blackPieces = (invert pieces `above` blackPawns) `over` emptyRows


-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x)
