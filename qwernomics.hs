module Qwernomics where
import Data.List (elemIndex)
import Data.Maybe (fromJust)
--import Dictionary      --optional,  58K-word dictionary
--import DictionaryHuge  --optional, 466K-word dictionary

alpha  = ['a'..'z']
qwerty = ['q','w','e','r','t','y','u','i','o','p','a','s','d','f','g','h','j','k','l','z','x','c','v','b','n','m']
dvorak = ['p','y','f','g','c','r','l','a','o','e','u','i','d','h','t','n','s','q','j','k','x','b','m','w','v','z']

--finds keys that are in the same place in two keyboards
fixpoints xs ys = map fst $ filter (\(a,b) -> a==b) $ zip xs ys
--fixpoints qwerty dvorak  == 'x','o','d'
--fixpoints alpha  dvorak  == 'z'

convert xs ys []     = []
convert xs ys (z:zs) = (toEnum (fromEnum z + shift) :: Char) : convert xs ys zs
                        where kNums = zipWith (-) (map fromEnum ys) (map fromEnum xs)
                              shift = kNums !! fromJust (elemIndex z xs)
-- convert alpha qwerty ['a'..'f']
-- convert qwerty alpha "qwerty"
-- convert dvorak alpha "pyfgcr"

-- specialized version of convert to make it faster (for dictionary search); note ys is removed
convert' xs shifts []     = []
convert' xs shifts (z:zs) = (toEnum (fromEnum z + shift) :: Char) : convert' xs shifts zs
                                     where shift = shifts !! fromJust (elemIndex z xs)

--For inverse transformations, just use negate
qwertyToDvorak = zipWith (-) (map fromEnum dvorak) (map fromEnum qwerty)
alphaToQwerty  = zipWith (-) (map fromEnum qwerty) (map fromEnum alpha)
alphaToDvorak  = zipWith (-) (map fromEnum dvorak) (map fromEnum alpha)

--FORBIDDEN KEYBOARDS (e.g. forbiddenDvorak 2 -> dvorak-squared)
forbiddenDvorak n
  | (n<=0)    = iterate (convert dvorak qwerty) dvorak !! (abs n + 1)
  | otherwise = iterate (convert qwerty dvorak) dvorak !! (n-1)
-- fmap (+2) $ elemIndex dvorak $ map forbiddenDvorak [2..250]
-- forbiddenDvorak 210  ==  "qwertyuiopasdfghjklzxcvbnm"  ==  forbiddenDvorak (-210)
-- forbiddenDvorak 211  ==  "pyfgcrlaoeuidhtnsqjkxbmwvz"  ==  forbiddenDvorak (-209)

forbiddenQwerty n
  | (n<=0)    = iterate (convert qwerty alpha) qwerty !! (abs n + 1)
  | otherwise = iterate (convert alpha qwerty) qwerty !! (n-1)
-- fmap (+2) $ elemIndex qwerty $ map forbiddenQwerty [2..250]
-- forbiddenQwerty 42  ==  "abcdefghijklmnopqrstuvwxyz"  ==  forbiddenQwerty (-42)
-- forbiddenQwerty 43  ==  "qwertyuiopasdfghjklzxcvbnm"  ==  forbiddenQwerty (-41)

forbiddenAlpha n = iterate (convert alpha dvorak) alpha !! (n-1)
-- fmap (+2) $ elemIndex alpha $ map forbiddenAlpha [2..1000]
-- forbiddenAlpha 925 == "abcdefghijklmnopqrstuvwxyz"