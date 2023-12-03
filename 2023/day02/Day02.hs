
module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)

replace :: String -> T.Text -> T.Text
replace a = T.replace space blank
    where space = T.pack a
          blank = T.pack ""

replaceComma :: T.Text -> T.Text
replaceComma = replace ","

splitOn' :: String -> T.Text -> [T.Text]
splitOn' x = T.splitOn (T.pack x)
splitOnColon = splitOn' ":"
splitOnSemicolon = splitOn' ";"

getElements :: [Int] -> [a] -> [a]
getElements _ [] = []
getElements [] _ = []
getElements (x:xs) y = y !! x : getElements xs y

convertToMap :: Ord a => [a] -> M.Map a a
convertToMap x = 
    let len = length x
        colors = getElements [0,2..len-1] x
        values = getElements [1,3..len-1] x
    in  M.fromList $ zip values colors

parseInput :: [T.Text] -> [[M.Map String String]]
parseInput input = 
    let rawSets = map (splitOnSemicolon . replaceComma . (!! 0) . tail . splitOnColon) input
        splitAndUnpack = words . T.unpack
        rawSets' = map (map $ convertToMap . splitAndUnpack) rawSets
    in  rawSets'

lookupFromMaybe :: Ord k => k -> M.Map k String -> Int
lookupFromMaybe color = read @Int . fromMaybe "0" . M.lookup color

getColorValue :: M.Map String String -> [Int]
getColorValue x = [
        lookupFromMaybe "red" x, 
        lookupFromMaybe "green" x,
        lookupFromMaybe "blue" x
    ]

checkCubeSet :: (Ord a, Num a) => [a] -> Bool
checkCubeSet x = r > redLimit || g > greenLimit || b > blueLimit
    where (r, g, b) = (x!!0, x!!1, x!!2)
          (redLimit, greenLimit, blueLimit) = (12, 13, 14)

part1 :: Foldable t => [t (M.Map String String)] -> Int
part1 x = 
    let checks = map (any (checkCubeSet . getColorValue)) x
        indices = L.elemIndices False checks
    in  sum $ map (+1) indices

part2 :: [[M.Map String String]] -> Int
part2 x = 
    let byColor = map (L.transpose . map getColorValue) x
        maxByColor = map (product . map maximum) byColor
    in  sum maxByColor

main :: IO()
main = do
    input <- parseInput . T.lines . T.pack <$> readFile "InputDay02.txt"
    print $ part1 input
    print $ part2 input
