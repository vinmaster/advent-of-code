import           Data.List                      ( group
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map

parseInput :: String -> [String]
parseInput = (splitOn "-") . head . words

haveAtLeastDouble :: String -> Bool
haveAtLeastDouble = any (>= 2) . map length . group

noDecreasing :: String -> Bool
noDecreasing n = n == sort n

haveExactlyDouble :: String -> Bool
haveExactlyDouble = any (== 2) . map length . group

part1 :: [String] -> Int
part1 [num1, num2] = length $ filter criterias combinations
 where
  combinations = map show $ [(read num1 :: Int) .. (read num2 :: Int)]
  criterias    = (\n -> haveAtLeastDouble n && noDecreasing n)

part2 :: [String] -> Int
part2 [num1, num2] = length $ filter criterias combinations
 where
  combinations = map show $ [(read num1 :: Int) .. (read num2 :: Int)]
  criterias    = (\n -> haveExactlyDouble n && noDecreasing n)

main :: IO ()
main = do
  let dayNum = "4"
  input <- parseInput <$> readFile ("2019/day" ++ dayNum ++ "/input.txt")
  putStrLn $ "day" ++ dayNum ++ " part1: " ++ show (part1 input)
  putStrLn $ "day" ++ dayNum ++ " part2: " ++ show (part2 input)

{-

bothParts :: (Int, Int) -> [Int]
bothParts (start, end) = map (length . (flip filter) common) [multiple, double] 
        where
            increasingDigits xs = sort xs == xs
            multiple   = any (>1)  . map length . group
            double     = any (==2) . map length . group
            common     = filter increasingDigits . map show $ [start..end]

--------------------------------------------------------------------------------

import Data.List (sort, group)

isNotDescending x = sort x == x
hasDouble = any (>=2) . map length . group
hasOneExactlyDoubledDigit = elem 2 . map length . group

day04 =
  let ws = filter isNotDescending . map show $ [272091..815432] in
  (length . filter hasDouble $ ws, length . filter hasOneExactlyDoubledDigit $ ws)

--------------------------------------------------------------------------------

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List (group)

type Input = (Int, Int)

part1 :: Input -> Int
part1 (lo, hi) = length [n | n <- [lo..hi],
                         let pairs = zip <*> tail $ show n,
                         any (uncurry (==)) pairs,
                         all (uncurry (<=)) pairs]

part2 :: Input -> Int
part2 (lo, hi) = length [n | n <- [lo..hi],
                         let pairs = zip <*> tail $ show n,
                         all (uncurry (<=)) pairs,
                         any ((== 2) . length) $ group (show n)]

prepare :: String -> Input
prepare input = case break (== '-') . filter (not . isSpace) $ input of
  (a, (_:b)) -> (read a, read b)
  _ -> error "Malformed input"

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

-}
