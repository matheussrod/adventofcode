
module Main where

import Data.Char ( isDigit )
import Data.List ( isPrefixOf )

validDigits :: String -> String
validDigits = filter isDigit

headAndLast :: [a] -> [a]
headAndLast x = [head x, last x]

toInt :: String -> Int
toInt = read

part1 :: [String] -> Int
part1 = sum . map (toInt . headAndLast . validDigits)

wordToNumber :: String -> String
wordToNumber str
  | "one" `isPrefixOf` str = '1' : wordToNumber (drop 1 str)
  | "two" `isPrefixOf` str = '2' : wordToNumber (drop 1 str)
  | "three" `isPrefixOf` str = '3' : wordToNumber (drop 1 str)
  | "four" `isPrefixOf` str = '4' : wordToNumber (drop 1 str)
  | "five" `isPrefixOf` str = '5' : wordToNumber (drop 1 str)
  | "six" `isPrefixOf` str = '6' : wordToNumber (drop 1 str)
  | "seven" `isPrefixOf` str = '7' : wordToNumber (drop 1 str)
  | "eight" `isPrefixOf` str = '8' : wordToNumber (drop 1 str)
  | "nine" `isPrefixOf` str = '9' : wordToNumber (drop 1 str)
  | otherwise = case str of
      [] -> []
      (c:cs) -> c : wordToNumber cs

part2 :: [String] -> Int
part2 = part1 . map wordToNumber

main :: IO ()
main = do
    input <- lines <$> readFile "InputDay01.txt"
    print $ part1 input
    print $ part2 input
