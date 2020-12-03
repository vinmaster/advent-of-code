import           System.Environment
import           System.Process
import           Text.Printf

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then error "Must supply year and day" else pure ()
  let [year, day] = args
  putStrLn $ printf "Running year %s day %s" year day
  callCommand $ printf "stack runghc -- %s/day%s/day%s.hs" year day day
