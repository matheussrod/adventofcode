
module Main where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.List (nub)

extractSymbols :: String -> String
extractSymbols = nub . filter (`notElem` "0123456789.")

symbolsPosition :: String -> String -> [Int]
symbolsPosition symbols x = 
    [ idx 
    | (idx, char) <- zip [0..] x
    , char `elem` symbols
    ]

buildKernel :: Int -> [Int] -> [Int]
buildKernel len x =  
    let kernel = [(-len)-1, -len, (-len)+1, -1, 1, len-1, len, len+1]
        kernelIndices = [ x' + y | y <- kernel, x' <- x]
        validKernelIndices = filter (>= 0) kernelIndices
    in  validKernelIndices

extractNumbers :: [Char] -> [(Int, Int, Int)]
extractNumbers str = go str 0 []
    where
    go [] _ acc = reverse acc
    go (c:cs) idx acc
        | isDigit c = 
            let (num, rest) = span isDigit (c:cs)
                numVal = read num :: Int
                endIdx = idx + length num - 1
            in  go rest (endIdx + 1) ((idx, endIdx, numVal):acc)
        | otherwise = go cs (idx + 1) acc

checkNumber :: [Int] -> (Int, Int, Int) -> Maybe Int
checkNumber kernel number = 
    let (start, end, value) = number
        sequence = [start..end]
        check = any (`elem` kernel) sequence
        value' = if check then Just value else Nothing
    in  value'

lenPosNumbers :: Integral a => String -> String -> (a, [Int], [(Int, Int, Int)])
lenPosNumbers symbols input = 
    let len = round (sqrt (fromIntegral $ length input))
        positions = symbolsPosition symbols input
        numbers = extractNumbers input
    in  (len, positions, numbers)

part1 :: String -> String -> Int
part1 symbols input = 
    let (len, positions, numbers) = lenPosNumbers symbols input
        kernel = buildKernel len positions
        validNumbers = map (checkNumber kernel) numbers
        validNumbersInt = map (fromMaybe 0) validNumbers
    in  sum validNumbersInt

part2 :: String -> String -> Int
part2 symbols input = 
    let (len, positions, numbers) = lenPosNumbers symbols input
        kernel = [buildKernel len [p] | p <- positions]
        validNumbers = map (\y -> map (checkNumber y) numbers) kernel
        countJusts = map (length . filter isJust)
        numbersCount = countJusts validNumbers
        numberCountLenght = zip numbersCount validNumbers
        numbersTwo = map snd $ filter (\x -> fst x == 2) numberCountLenght
    in  sum $ map (product . map (fromMaybe 1)) numbersTwo

main :: IO ()
main = do
    input <- concat . lines <$> readFile "InputDay03.txt"
    let symbols = extractSymbols input
    print $ part1 symbols input
    print $ part2 "*" input
