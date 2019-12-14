
main :: IO ()
main = do
  let dayNum = "8"
  putStrLn ("TODO " ++ dayNum)
  -- input <- parseInput <$> readFile ("2019/day" ++ dayNum ++ "/input.txt")
  -- putStrLn $ "day" ++ dayNum ++ " part1: " ++ show (part1 input)
  -- putStrLn $ "day" ++ dayNum ++ " part2: " ++ show (part2 input)

{-

count :: Int -> [Int] -> Int
count i = length . filter (i ==)

part1 :: [[Int]] -> Int
part1 xs = count 1 l * count 2 l
    where l = minimumBy (comparing (count 0)) xs

part2 :: [[Int]] -> [[Int]]
part2 layers = chunksOf 25 [f x |x <- transpose layers]
    where f = head . dropWhile (2 ==)

main :: IO ()
main = do
    infile <- readFile "day8.txt"
    let layers = chunksOf (25 * 6) [digitToInt c | c <- init infile]
    print $ part1 layers
    mapM_ (putStrLn . map intToDigit) $ part2 layers

--------------------------------------------------------------------------------
    
solve1 = count 1 layer * count 2 layer
  where
    layer = minimumBy (comparing (count 0)) (splitFile 25 6 input)
    count n = length . filter (==n) . concat
solve2 = printImage $ foldr1 zipLayer $ splitFile 25 6 input
splitFile row col = fmap (chunksOf  row) . chunksOf (row * col)
printImage = mapM_ putStrLn . fmap (fmap step)
  where
    step 0 = ' '
    step 1 = '#'
    step 2 = '*'
zipLayer = zipWith (zipWith step)
  where
    step 2 a = a
    step a _ = a

--------------------------------------------------------------------------------

w = 25
h = 6

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

-- find the nonTransparent pixel by checking every w*h-th pixel
nonTransparent :: [Char] -> Char
nonTransparent xs | head xs /= '2' = head xs
                  | otherwise = nonTransparent $ drop (w*h) xs

draw :: [Char] -> [[Char]]
draw pixels = [concatMap drawPixel row
              | i <- [0 .. (h - 1)]
              , let row = take w $ drop (i * w) pixels] 
drawPixel x = if x == '1' then "# " else "  "

main = do
   pixels <- getLine
   -- Part 1
   let results = [(c '0', c '1' * c '2')
                 | i <- [0, w*h .. (length pixels - 1)]
                 , let layer = take (w*h) $ drop i pixels
                 , let c x = count x layer]
   putStrLn $ show $ minimum results 
   -- Part 2
   putStrLn $ unlines $ draw  [nonTransparent $ drop i pixels
                              | i <- [0 .. w*h-1]]


--------------------------------------------------------------------------------

{- stack script
    --resolver lts-14.12
-}

import Control.Monad (msum)
import Data.Char (digitToInt)
import Data.List (minimumBy, transpose)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- map digitToInt . head . lines <$> readFile "Day8.txt"

  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 = checksum . getLayerWithLeastZeros . getLayers
  where
    countDigits n = length . filter (== n)
    getLayerWithLeastZeros = minimumBy (comparing (countDigits 0))
    checksum layer = countDigits 1 layer * countDigits 2 layer

part2 :: [Int] -> Image
part2 = parseImage . map (map decodeColor) . getLayers

-- Layer utilities

getLayers :: [Int] -> [[Int]]
getLayers = chunksOf layerLength
  where
    layerLength = layerWidth * layerHeight

layerWidth, layerHeight :: Int
layerWidth = 25
layerHeight = 6

-- Color

data Color = White | Black

decodeColor :: Int -> Maybe Color
decodeColor 0 = Just Black
decodeColor 1 = Just White
decodeColor 2 = Nothing
decodeColor n = error $ "Invalid color: " ++ show n

-- Image

newtype Image = Image { unImage :: [[Color]] }

parseImage :: [[Maybe Color]] -> Image
parseImage = Image . chunksOf layerWidth . map (fromJust . msum) . transpose

instance Show Image where
  show = unlines . map (map showColor) . unImage
    where
      showColor White = 'X'
      showColor Black = ' '

-}
