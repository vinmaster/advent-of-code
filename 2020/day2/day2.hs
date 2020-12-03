import Debug.Trace

parseInput :: String -> [String]
parseInput = lines

-- Split string s with char c
splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy c s  =
  let
    i = (length . takeWhile (/= c)) s
    (as, bs) = splitAt i s
  in as : splitBy c (if bs == [] then [] else tail bs)

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

-- Count the occurrence of char c in string str
occurrence :: String -> Char -> Int
occurrence str c = length $ filter (== c) str

-- Check if num is between min and max
between :: (Int, Int) -> Int -> Bool
between (min, max) num
  | min <= num && num <= max = True
  | otherwise = False

parseEntry :: String -> (Int, Int, Char, String)
parseEntry entry = (read num1, read num2, char', password)
  where [policy, char, password] = words entry
        [num1, num2] = splitBy '-' policy
        char' = char !! 0

part1 :: [String] -> Int
part1 input = length $ filter isValid $ map parseEntry input
  where isValid = (\(min, max, char, password) -> between (min, max) $ occurrence password char)

part2 :: [String] -> Int
part2 input = length $ filter isValid $ map parseEntry input
  where isValid = (\(index1, index2, char, password) -> 
                  (password !! (index1 - 1) == char) `xor` (password !! (index2 - 1) == char))

main :: IO ()
main = do
  input <- parseInput <$> readFile "2020/day2/input.txt"
  putStrLn $ "day2 part1: " ++ show (part1 input)
  putStrLn $ "day2 part2: " ++ show (part2 input)

{-
read' :: [String] -> (Int, Int, String, String)
read' [a,b,c,d] = (read a, read b, c, d)

isValid :: (Int, Int, String, String) -> Bool
isValid (min, max, c, d) = min <= count && count <= max
    where count = length [x | x <- d, [x] == c]

isValid2 :: (Int, Int, String, String) -> Bool
isValid2 (a, b, c, d) = 
    d !! a' == c' && d !! b' /= c' 
    || (d !! a' /= c' && d !! b' == c')
    where (a', b', c') = (a - 1, b - 1, head c)

main :: IO ()
main = do 
  infile <- readFile "./resources/day2.in"
  let input = [read' (words xs) | xs <- lines infile]
  -- part 1 
  print . length $ filter isValid input
  -- part 2
  print $ length $ filter isValid2 input

--------------------------------------------------------------------------------

module Main where

import Data.List.Split (splitOn)
import Text.Regex.TDFA
import Control.Lens

main :: IO ()
main = do
    content <- lines <$> readFile "input.txt"
    let asLines = map toLine content
    print $ length $ filter part1 asLines
    print $ length $ filter part2 asLines

type Line = (Int, Int, Char, String)

toLine :: String -> Line
toLine str = (read $ head list, read $ list !! 1, head $ list !! 2, last list)
    where list = (str =~ lineRegex :: (String, String, String, [String])) ^. _4

part1 :: Line -> Bool
part1 (min, max, char, str) = count >= min && count <= max
    where count = length $ filter (==char) str

part2 :: Line -> Bool
part2 (one, two, char, str) = (char == str!!(one - 1)) /= (char == str!!(two - 1))

lineRegex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"

--------------------------------------------------------------------------------

-- Part 1 --

type Line = (Int,Int,Char,String)

parseLine :: String -> Line
parseLine str = (read minStr, read maxStr, head letterStr, drop 2 rest3)
  where
    (minStr, rest1) = break (== '-') str
    (maxStr, rest2) = break (== ' ') (tail rest1)
    (letterStr, rest3) = break (== ':') (tail rest2)

isValid :: Line -> Bool
isValid (pos1,pos2,letter,password) =
  (password !! (pos1 - 1) == letter) ^ (password !! (pos2 - 1) == letter)
    where x ^ y = x && not y || not x && y

countValid :: [Line] -> Int
countValid = length . filter id . map isValid

main :: IO ()
main = interact $ show . countValid . map parseLine . lines


-- Part 2 --

type Line = (Int,Int,Char,String)

parseLine :: String -> Line
parseLine str = (read minStr, read maxStr, head letterStr, drop 2 rest3)
  where
    (minStr, rest1) = break (== '-') str
    (maxStr, rest2) = break (== ' ') (tail rest1)
    (letterStr, rest3) = break (== ':') (tail rest2)

isValid :: Line -> Bool
isValid (pos1,pos2,letter,password) =
  (password !! (pos1 - 1) == letter) ^ (password !! (pos2 - 1) == letter)
    where x ^ y = x && not y || not x && y

countValid :: [Line] -> Int
countValid = length . filter id . map isValid

main :: IO ()
main = interact $ show . countValid . map parseLine . lines
-}
