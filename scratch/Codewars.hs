{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codewars where

-- import Data.Tuple ( fst, swap )
-- import Data.Map.Strict ( Map, empty, toList, insertWith ) 
-- import Data.List ( maximumBy )

-- highestRank :: Ord c => [c] -> c
-- highestRank = snd . maximum . map swap . toList . foldr (\k -> insertWith (+) k 1) empty

-- digpow :: Integer -> Integer -> Integer
-- digpow n p = let val = sum $ zipWith (^) (read . pure <$> show n) [p..] in 
--     if val `mod` n == 0 then val `div` n else -1

-- digitalRoot :: Integral a => a -> a
-- digitalRoot n = if n < 10 then n else digitalRoot $ sum (digits n)
--     where digits = map (`mod` 10) . takeWhile (/= 0) . iterate (`div` 10)

-- import Control.Monad ( foldM )
-- import Data.Maybe ( isJust )

-- tickets :: [Int] -> Bool
-- tickets = isJust . foldM makeChange (0, 0)

-- makeChange (x, y) 25 = Just (x + 1, y)
-- makeChange (0, y) _ = Nothing
-- makeChange (x, y) 50 = Just (x - 1, y + 1)
-- makeChange (x, y) 100
--     | y > 0 = Just (x - 1, y - 1)
--     | x > 2 = Just (x - 3, y)
-- makeChange _ _ = Nothing


-- import Data.Char ( isDigit ) 
-- import Data.List ( find, sortOn ) 

-- yourOrderPelase = unwords . sortOn (find isDigit) . words

-- import Data.Char ( toLower, toUpper )
-- import Data.List ( intercalate )

-- accum :: [Char] -> [Char]
-- accum = intercalate "-" . zipWith rep [0..]
--     where rep n l = toUpper l : replicate n (toLower l)

-- type Garden = [String]

-- crap :: Garden -> Int -> Int -> String
-- crap g b c 
--     | 'D' `elem` unlines g = "Dog!!"
--     | b * c >= length (filter (== '@') $ unlines g) = "Clean"
--     | otherwise = "Cr@p"

-- makeTower n = map (towerLevel n) [1..n]

-- towerLevel n floor = let margin = replicate (n - floor) ' ' in 
--     margin ++ replicate (2 * floor - 1) '*' ++ margin

-- import Data.Char ( toUpper )
-- import Data.List ( intersperse )

-- getInitials :: String -> String
-- getInitials = intersperse '.' . map (toUpper . head) . words

-- getSum a b = sum $ if a < b then [a..b] else [b..a]

-- import Data.Maybe
-- import Data.List

-- findNeedle :: [String] -> String
-- findNeedle = (++) "Found needle at position " . show . fromJust . elemIndex "needle"

-- musicalOCD :: Int -> Int
-- musicalOCD = min 1 . (`rem` 3)


-- import Data.List ( nub )
-- import Data.Char ( toLower )

-- isIsogram :: String -> Bool
-- isIsogram s = length s == length (nub $ toLower <$> s)

-- import Data.Map.Strict ((!?), Map, delete, fromList, toAscList)

-- houseCoords = fromList . concat . zipWith line [0..] . lines
--     where line y = map (\(x, c) -> (c, (x, y))) . filter (\(_, c) -> c /= '.') . zip [0..]

-- houseDist pos = fst . foldl f (0, pos) . map snd . toAscList
--   where f (d, last) next = (d + dist last next, next)
--         dist (a, b) (x, y) = abs (a - x) + abs (b - y)

-- distributeGifts :: String -> Either String Int
-- distributeGifts smap = maybe (Left "Where is Santa Claus?") Right $
--     let houseMap = houseCoords smap
--         houses = delete 's' houseMap in
--             flip houseDist houses <$> houseMap !? 's'

-- import Data.Bifunctor

-- solution = snd . (em >.= circ 100 "CDM" >.= circ 10 "XLC" >.= circ 1 "IVX")

-- em n = (n `mod` 1000, replicate (fromIntegral $ n `quot` 1000) 'M')

-- (>.=) f g x = let (next, s) = f x in second (s++) (g next)
-- infixl 1 >.=

-- circ order chars@[oneC, fiveC, tenC] value
--     | value < order * 4  = (value `rem` order, replicate (fromIntegral $ value `quot` order) oneC)
--     | value < order * 5  = (value `rem` order, [oneC, fiveC])
--     | value < order * 9  = second (fiveC :) (circ order chars (value - 5 * order))
--     | value < order * 10 = second ([oneC, tenC] ++) (circ order chars (value `rem` order))

-- import Data.Char

-- alphaDict = [reverse (str x) | x <- [0..]]

-- char n = if n < 26 then chr (n + ord 'a') else chr (n + ord 'A' - 26)

-- str n = char (n `mod` 52) : (if n > 51 then str (n `div` 52 - 1) else [])

-- import Data.List

-- maxSequence :: [Int] -> Int
-- maxSequence s = let sums = map sum $ substrings s in 
--     if all (<0) s || null sums then 0 else maximum sums

-- substrings [] = []
-- substrings ls = tail (inits ls) ++ substrings (tail ls)

