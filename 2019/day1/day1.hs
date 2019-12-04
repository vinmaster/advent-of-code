parseInput :: String -> [Integer]
parseInput = map read . lines

calculateFuel :: Integer -> Integer
calculateFuel mass = (mass `div` 3) - 2

calculateTotalFuel :: Integer -> Integer
calculateTotalFuel mass = do
  let fuel = calculateFuel mass
  if fuel < 0 then 0 else fuel + (calculateTotalFuel $ fuel)

part1 :: [Integer] -> Integer
part1 input = sum $ map calculateFuel input

part2 :: [Integer] -> Integer
part2 input = sum $ map calculateTotalFuel input

main :: IO ()
main = do
  input <- parseInput <$> readFile "2019/day1/input.txt"
  putStrLn $ "day1 part1: " ++ show (part1 input)
  putStrLn $ "day1 part2: " ++ show (part2 input)

{-

calculateTotalFuel mass | fuel < 0  = 0
                        | otherwise = fuel + (calculateTotalFuel $ fuel)
  where fuel = calculateFuel mass

--------------------------------------------------------------------------------

calculateTotalFuel = sum . takeWhile (> 0) . drop 1 . iterate calculateFuel

--------------------------------------------------------------------------------

calculateTotalFuel = sum . tail . takeWhile (> 0) . iterate calculateFuel

-}
