import           Data.List.Split
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( update
                                                , fromList
                                                )

parseInput :: String -> [Int]
parseInput = map read . (splitOn ",")

replace :: [a] -> (Int, a) -> [a]
replace list (index, newElement) = front ++ [newElement] ++ back
 where
  front      = fst $ splitAt (fromIntegral index) list
  (x : back) = snd $ splitAt (fromIntegral index) list

replace' :: [a] -> (Int, a) -> [a]
replace' xs (i, a) = take i xs ++ a : drop (i + 1) xs

runProgram :: [Int] -> [Int]
runProgram program = runProgram' 0 program

runProgram' :: Int -> [Int] -> [Int]
runProgram' i program
  | opcode == 1
  = runProgram' (i + 4) $ toList $ update outputIndex sum $ fromList program
  | opcode == 2
  = runProgram' (i + 4) $ toList $ update outputIndex product $ fromList program
  | otherwise
  = program
 where
  opcode      = program !! i
  a           = program !! (program !! (i + 1))
  b           = program !! (program !! (i + 2))
  outputIndex = program !! (i + 3)
  sum         = a + b
  product     = a * b

part1 :: [Int] -> Int
part1 input = head $ runProgram correctedProgram
  -- where correctedProgram = replace (replace input (1, 12)) (2, 2)
  where correctedProgram = toList $ update 2 2 $ update 1 12 $ fromList input

part2 :: [Int] -> Int
part2 input = (noun * 100) + verb
 where
  (_, noun, verb) = matched
  matched         = head $ filter targetFn combinations
  targetFn        = (\(p, noun, verb) -> (== 19690720) $ head $ runProgram p)
  combinations =
    [ (toList $ update 2 verb $ update 1 noun $ fromList input, noun, verb)
    | noun <- [0 .. 99]
    , verb <- [0 .. 99]
    ]

main :: IO ()
main = do
  input <- parseInput <$> readFile "2019/day2/input.txt"
  putStrLn $ "day2 part1: " ++ show (part1 input)
  putStrLn $ "day2 part2: " ++ show (part2 input)

{-

import Data.List.Split
import Data.Sequence
import Data.Foldable
main = do
    input <- readFile "2-input"
    let instructions = fromList $ map read $ splitOn "," input
        fixedInstructions = update 2 2 $ update 1 12 instructions
        result = program 0 fixedInstructions
    putStrLn . show . head $ toList result
program ::Int -> Seq Int -> Seq Int
program n mem        
    | instruction == 1  = program (n+4) $ update adress (a+b) mem
    | instruction == 2  = program (n+4) $ update adress (a*b) mem
    | otherwise         = mem
    where instruction = mem `index` n
          a           = mem `index` (mem `index` (n+1))
          b           = mem `index` (mem `index` (n+2))
          adress      = mem `index` (n+3)

--------------------------------------------------------------------------------

import Commons

parseInput :: String -> [Int]
parseInput = map read . (splitOn (==','))

run :: Int -> [Int] -> Int
run p xs | code == 99 = head xs
         | otherwise  = run (p+4) xs'
         where
          code = xs !! p
          l    = xs !! (xs !! (p+1))
          r    = xs !! (xs !! (p+2))
          idx  = xs !! (p+3)
          op   = case code of
                  1 -> (+)
                  2 -> (*)
          xs'  = take idx xs ++ op l r : drop (idx+1) xs

reset :: a -> a -> [a] -> [a]
reset x y xs = [head xs, x, y] ++ drop 3 xs

day02b :: Int -> [Int] -> Int
day02b ex xs = 100 * noun + verb
  where
    g        = flip reset 0
    f x      = run 0 $ g x xs
    (x',_)   = head $ dropWhile (\(_,v) -> ex > v) [(x, f x) | x <- [0..]]
    noun     = x' - 1
    g'       = reset noun
    f' x     = run 0 $ g' x xs
    (verb,_) = head $ dropWhile (\(_,v) -> ex /= v) [(x, f' x) | x <- [0..]]

solution :: IO()
solution = do putStr "Part 01: "              
              intcodes <- parseInput <$> getInput "input02.txt"
              print $ run 0 (reset 12 2 intcodes)
              putStr "Part 02: "
              print $ day02b 19690720 intcodes

main :: IO()
main = solution

-}
