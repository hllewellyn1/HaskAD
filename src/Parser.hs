module Parser where

import Data.List
import Data.List.Split
import Data.Char

---------------------------------------------------------------

{-

This module contains a sort of simple parser.  It doesn't really use any 
proper parsing techniques, it's more of an algorithm to convert text (which
we limit to just digits, '-' and '.') into a floating point number.

The functions are a bit messy (because this is a rather messy way of
'parsing'), but it works really quite well.

-}

---------------------------------------------------------------

-- we take a list of characters (e.g. "12.5") and split them at '.' (i.e. ("12","5"))

convertCharTuple :: ([Char],[Char]) -> ([Float],[Float])
convertCharTuple (x,y) = (map fromIntegral (map digitToInt (filter (/= '-') x)), map fromIntegral (map digitToInt (filter (/= '-') y)))

-- we apply this function to the first tuple element

abovePlaceMultiplier :: [Float]
abovePlaceMultiplier = map (10^) [0,1..]

-- we apply this function to the second tuple element

belowPlaceMultiplier :: [Float]
belowPlaceMultiplier = map recip (map (10^) [1..])

-- to do this, we define this elementwise list multiplication function

listMultiply :: ([Float],[Float]) -> [Float]
listMultiply (x:[],y:[]) = [x*y]
listMultiply (x:[],y:ys) = [x*y]
listMultiply (x:xs,y:[]) = [x*y]
listMultiply (x:xs,y:ys) = (x*y):(listMultiply (xs,ys)) 
listMultiply ([],_) = []
listMultiply (_,[]) = []

-- combineToFloat is a composition of the above

combineToFloat :: ([Float],[Float]) -> Float
combineToFloat (x,y) = sum ((listMultiply (reverse x, abovePlaceMultiplier))++(listMultiply (y, belowPlaceMultiplier)))

-- self explanatory (most patterns will not be used, but GHC doesn't know this)

listToTuple :: [[Char]] -> ([Char],[Char])
listToTuple [] = ([],[])
listToTuple [[]] = ([],[])
listToTuple [x] = (x,[])
listToTuple [x,y] = (x,y)

-- self explanatory

checkNegative :: [Char] -> Float -> Float
checkNegative x y
	| (elem '-' x)  = (-y)
	| otherwise 	= (y)

-- composition of all of the above

stringToFloat :: [Char] -> Float
stringToFloat x = checkNegative x (combineToFloat (convertCharTuple (listToTuple (splitOn "." x))))






