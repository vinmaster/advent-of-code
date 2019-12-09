import           Debug.Trace
import           Data.List                      ( intersect )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map

type Coord = (Int, Int)

addCoords :: Coord -> Coord -> Coord
a `addCoords` b = foldl (\(accX, accY) (x, y) -> (accX + x, accY + y)) a [b]

parseInput :: String -> ([String], [String])
parseInput input = (wire1, wire2)
  where [wire1, wire2] = take 2 $ map (splitOn ",") $ lines input

deltaCoord :: String -> Coord
deltaCoord dir = case dir of
  "U" -> (0, 1)
  "D" -> (0, -1)
  "L" -> (-1, 0)
  "R" -> (1, 0)

wireCoords :: Coord -> [String] -> [Coord]
wireCoords starting instructions = concat
  $ scanl nextPathCoords [starting] instructions
 where
  nextPathCoords prevPathCoords instruction =
    tail . take (distance + 1) $ iterate (addCoords delta) lastCoord
   where
    delta              = deltaCoord dir
    (dir, distanceStr) = splitAt 1 instruction
    distance           = read distanceStr :: Int
    lastCoord          = last prevPathCoords

part1 :: ([String], [String]) -> Int
part1 (wire1Instructions, wire2Instructions) =
  minimum . map manhattanDistance $ tail $ Map.keys $ Map.intersection
    wire1Map
    wire2Map
 where
  manhattanDistance (x, y) = abs x + abs y
  wire1Map    = Map.fromList $ zip wire1Coords [0 ..]
  wire2Map    = Map.fromList $ zip wire2Coords [0 ..]
  wire1Coords = wireCoords (0, 0) wire1Instructions
  wire2Coords = wireCoords (0, 0) wire2Instructions

part2 :: ([String], [String]) -> Int
part2 (wire1Instructions, wire2Instructions) =
  minimum $ tail $ Map.elems $ Map.intersectionWith (+) wire1Map wire2Map
 where
  manhattanDistance (x, y) = abs x + abs y
  wire1Map    = Map.fromList $ zip wire1Coords [0 ..]
  wire2Map    = Map.fromList $ zip wire2Coords [0 ..]
  wire1Coords = wireCoords (0, 0) wire1Instructions
  wire2Coords = wireCoords (0, 0) wire2Instructions

main :: IO ()
main = do
  let dayNum = "3"
  input <- parseInput <$> readFile ("2019/day" ++ dayNum ++ "/input.txt")
  putStrLn $ "day" ++ dayNum ++ " part1: " ++ show (part1 input)
  putStrLn $ "day" ++ dayNum ++ " part2: " ++ show (part2 input)

{-

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

parseWire :: String -> Map (Int, Int) Int
parseWire = M.fromListWith min . flip zip [1..] . tail
            . scanl move (0, 0) . concatMap expandSteps . splitOn ","
    where expandSteps (d:ds) = replicate (read ds) d
          expandSteps [] = error "Unable to parse instr"
          move (x, y) 'U' = (x, y+1)
          move (x, y) 'D' = (x, y-1)
          move (x, y) 'L' = (x-1, y)
          move (x, y) 'R' = (x+1, y)
          move _      _   = error "Unknown direction"

part1 :: String -> Int
part1 = minimum . map (\(x, y) -> abs x + abs y) . M.keys
        . foldr1 M.intersection . map parseWire . lines

part2 :: String -> Int
part2 = minimum . M.elems . foldr1 (M.intersectionWith (+)) . map parseWire . lines

--------------------------------------------------------------------------------

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "Day3.txt"
  let (cmds1, cmds2) = splitCommands input
      pointsMap1 = toPointsMap cmds1
      pointsMap2 = toPointsMap cmds2

  print $ part1 pointsMap1 pointsMap2
  print $ part2 pointsMap1 pointsMap2

part1 :: PointsMap -> PointsMap -> Int
part1 pointsMap1 pointsMap2 = minimum $ Map.intersectionWithKey manhattanDist pointsMap1 pointsMap2
  where
    manhattanDist (x, y) _ _ = abs x + abs y

part2 :: PointsMap -> PointsMap -> Int
part2 pointsMap1 pointsMap2 = minimum $ Map.intersectionWithKey addSteps pointsMap1 pointsMap2
  where
    addSteps _ steps1 steps2 = steps1 + steps2

-- ((dx, dy), number of steps)
type Command = ((Int, Int), Int)

-- A list of visited points, mapped to the number of steps it took to get there.
-- If a point was visited multiple times, keep the lowest number of steps
type PointsMap = Map (Int, Int) Int

splitCommands :: String -> ([Command], [Command])
splitCommands = toPair . map (map toCommand . splitOn ",") . lines
  where
    toPair = \case
      [x, y] -> (x, y)
      l -> error $ "Not a pair: " ++ show l

toCommand :: String -> Command
toCommand cmd = (dirSteps, read @Int $ tail cmd)
  where
    dirSteps = case head cmd of
      'R' -> (1, 0)
      'L' -> (-1, 0)
      'U' -> (0, 1)
      'D' -> (0, -1)
      dir -> error $ "Bad direction: " ++ [dir]

toPointsMap :: [Command] -> PointsMap
toPointsMap = Map.fromListWith const . getPoints ((0, 0), 0)
  where
    getPoints _ [] = []
    getPoints ((currX, currY), currSteps) (cmd:cmds) =
      let ((dx, dy), steps) = cmd
          points = flip map [1..steps] $ \step ->
            let stepX = currX + dx * step
                stepY = currY + dy * step
            in ((stepX, stepY), currSteps + step)
      in points ++ getPoints (last points) cmds

--------------------------------------------------------------------------------

module Main where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (uncons)

data Dir = U | D | L | R
  deriving Show

split :: String -> [(Dir, Int)]
split "" = []
split xs = let (start, rest) = span (/= ',') xs in
  case uncons start of
    Just (d, start') -> 
      let dir = case d of
                  'U' -> U
                  'D' -> D
                  'L' -> L
                  'R' -> R
      in (dir, read start') : split (drop 1 rest)
    Nothing -> []

fillGrid :: [(Dir, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
fillGrid = fillGrid' (0,0) 0
  where
    fillGrid' (x,y) s (instr:instrs) grid =
      let grid' = M.fromList $ case instr of
            (U, d) -> [((x, y+i), s + abs i) | i<-[1..d]]
            (D, d) -> [((x, y-i), s + abs i) | i<-[1..d]]
            (L, d) -> [((x-i, y), s + abs i) | i<-[1..d]]
            (R, d) -> [((x+i, y), s + abs i) | i<-[1..d]]
          pos' = case instr of
            (U, d) -> (x, y+d)
            (D, d) -> (x, y-d)
            (L, d) -> (x-d, y)
            (R, d) -> (x+d, y)
          s'  = s + snd instr
      in fillGrid' pos' s' instrs (grid <> grid')
    fillGrid' _ _ [] grid = grid

part1 :: [(Dir, Int)] -> [(Dir, Int)] -> Int
part1 first second = minimum [ abs x + abs y | (x,y) <- S.toList $ M.keysSet grid1 `S.intersection` M.keysSet grid2]
  where
    grid1 = fillGrid second mempty 
    grid2 = fillGrid first mempty

part2 :: [(Dir, Int)] -> [(Dir, Int)] -> Int
part2 first second = minimum [ d | (_, d) <- M.toList $ M.intersectionWith (+) grid1 grid2]
  where
    grid1 = fillGrid second mempty 
    grid2 = fillGrid first mempty

main :: IO ()
main = do
  input <- readFile "input.in"
  let [first, second] = split <$> lines input
  print $ part1 first second
  print $ part2 first second

-}
