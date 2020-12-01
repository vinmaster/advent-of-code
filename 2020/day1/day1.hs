parseInput :: String -> [Integer]
parseInput = map read . lines

part1 :: [Integer] -> Integer
part1 input = product . head $ [ [x1,x2] | x1 <- input, x2 <- input, x1 + x2 == 2020 ]
-- part1 input = head $ [ x1 * x2 | x1 <- input, x2 <- input, x1 + x2 == 2020 ]

part2 :: [Integer] -> Integer
part2 input = product . head $ [ [x1,x2,x3] | x1 <- input, x2 <- input, x3 <- input, x1 + x2 + x3 == 2020 ]

main :: IO ()
main = do
  input <- parseInput <$> readFile "2020/day1/input.txt"
  putStrLn $ "day1 part1: " ++ show (part1 input)
  putStrLn $ "day1 part2: " ++ show (part2 input)

{-
import Control.Applicative

main :: IO ()
main = do
    input <- readFile "Day01.txt"
    let numbers = map read $ lines input
    putStrLn $ show $ solve1 numbers
    putStrLn $ show $ solve2 numbers

solve1 numbers = x * y
    where (x,y) = head $
                filter (\(a,b) -> a + b == 2020) $
                liftA2 (,) numbers numbers 

solve2 numbers = x * y * z
    where (x,y,z) = head $
                filter (\(a,b,c) -> a + b + c == 2020) $
                liftA3 (,,) numbers numbers numbers

--------------------------------------------------------------------------------

inputData :: IO [Int]
inputData = map read . lines <$> input 20 1

sum2020 :: [Int] -> Maybe (Int, Int)
sum2020 (x : xs) = sum2020' x xs <|> sum2020 xs
  where
    sum2020' :: Int -> [Int] -> Maybe (Int, Int)
    sum2020' x xs = (x,) <$> find ((== 2020) . (+ x)) xs

part1 :: IO Int
part1 = uncurry (*) . fromMaybe (0, 0) . sum2020 <$> inputData

sum2020Three :: [Int] -> Maybe (Int, Int, Int)
sum2020Three (x : xs) = sum2020Three' x xs <|> sum2020Three xs
  where
    sum2020Three' :: Int -> [Int] -> Maybe (Int, Int, Int)
    sum2020Three' x (y : xs) = ((x,y,) <$> find ((== 2020) . (+ y) . (+ x)) xs) <|> sum2020Three' x xs
    sum2020Three' x [] = Nothing

part2 :: IO Int
part2 = (\(a, b, c) -> a * b * c) . fromMaybe (0, 0, 0) . sum2020Three <$> inputData
-}
