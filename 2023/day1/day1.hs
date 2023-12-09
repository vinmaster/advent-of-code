import Data.Char (isDigit, isLetter)
import Data.List (groupBy, partition)

parseInput :: String -> [String]
parseInput = lines

getFirstLastDigitAsInteger :: String -> Integer 
getFirstLastDigitAsInteger s = read firstLastDigit :: Integer 
  where
    nums = fst $ partition isDigit $ s
    firstLastDigit = sequence [head, last] nums 

part1 :: [String] -> Integer
part1 input = sum $ map getFirstLastDigitAsInteger input

-- part2 :: [String] -> Integer

main :: IO ()
main = do
  input <- parseInput <$> readFile "2023/day1/input.txt"
  putStrLn $ "part1: " ++ show (part1 input)
  -- putStrLn $ "part2: " ++ show (part2 input)
