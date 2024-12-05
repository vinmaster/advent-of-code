module Main where

import Data.List
import Debug.Trace

parseInt :: String -> Integer
parseInt str = read str :: Integer

parseLine :: String -> [Integer]
parseLine line = [left, right]
  where
    xs = splitBy ' ' line
    left = parseInt $ head xs
    right = parseInt $ last xs

parseInputString :: String -> [[Integer]]
parseInputString inputString = map parseLine (lines inputString)

-- https://stackoverflow.com/a/7569301
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

countOccurrences :: Integer -> [Integer] -> Integer
countOccurrences x = toInteger . length . filter (== x)

part1 :: [[Integer]] -> Integer
-- part1 input = traceShow distances head list1
part1 input = sum distances
  where
    list1 = sort $ (map head input)
    list2 = sort $ map (\xs -> xs !! 1) input
    distances = map (abs . uncurry subtract) (zip list1 list2)

part2 :: [[Integer]] -> Integer
part2 input = sum scores
  where
    list1 = map head input
    list2 = map (\xs -> xs !! 1) input
    scores = map (\x -> x * (countOccurrences x list2)) list1

main :: IO ()
main = do
  input <- parseInputString <$> readFile "2024/day1/input.txt"

  --   let input = parseInputString "3   4\n\
  -- \4   3\n\
  -- \2   5\n\
  -- \1   3\n\
  -- \3   9\n\
  -- \3   3"

  -- putStrLn $ show input
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)


{-
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

main = do
    numbers <- fmap words . lines <$> readFile "input1.txt"
    let list1 = fmap (read . head) numbers
    let list2 = fmap (read . (!!1)) numbers
    let answer1 = sum $ zipWith (\a b -> abs $ subtract a b) (sort list1) (sort list2)
    print answer1
    let counts = Map.fromListWith (+) (zip list2 (repeat 1))
    let answer2 = sum $ fmap (\x -> x * (fromMaybe 0 (Map.lookup x counts))) list1
    print answer2

--------------------------------------------------------------------------------


part1 = sum . map abs . (\[a, b] -> zipWith (-) a b) . map sort . transpose .  map (map read . words) . lines
part2 = sum . (\[a, b] -> map (\x -> (x *) . length . filter (== x) $ b) a) . transpose . map (map read . words) . lines
-}