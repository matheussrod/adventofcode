module Main where

import Data.List (intersect)

splitFirstOn :: Char -> String -> [String]
splitFirstOn c x = 
    let first = takeWhile (/=c) x
        second = drop 1 (dropWhile (/=c) x)
    in  [first, second]

parseInput :: String -> ([Int], [Int])
parseInput input = 
    let numbersString = last $ splitFirstOn ':' input
        numbers = splitFirstOn '|' numbersString
        winningNumbers = map (read @Int) $ words $ head numbers
        myNumbers = map (read @Int) $ words $ last numbers
    in  (winningNumbers, myNumbers)

getNumbers :: ([Int], [Int]) -> Int
getNumbers (w, m) = length $ w `intersect` m

part1 :: [Int] -> Int
part1 = sum . map (\x -> if x-1 < 0 then 0 else 2^(x-1))

part2 :: [Int] -> Int
part2 x = snd $ foldl copies (replicate (length x) 1, 0) x
    where 
        copies ([], tot) _ = ([], tot)
        copies (y:ys, tot) n = (zipWith (+) ys (replicate n y ++ repeat 0), tot + y)

main :: IO()
main = do
    input <- map parseInput . lines <$> readFile "InputDay04.txt"
    let numbers = map getNumbers input
    print $ part1 numbers
    print $ part2 numbers
