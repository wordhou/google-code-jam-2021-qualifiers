import Control.Monad
import Data.List

main = readLn >>= \t -> forM_ [1..t] $ \t' ->
  getParams >>= putStrLn . cs t' . disp . solve where
  cs t xs = "Case #" ++ show t ++ ": " ++ xs
  disp = show

getParams = do
  [x, y, s] <- words <$> getLine
  return (read x, read y, s) :: IO (Int, Int, String)

solve (x, y, s) = sum . snd $ mapAccumL f (head s) s where
  f '?' a = (a, 0)
  f 'C' 'J' = ('J', x)
  f 'C' _ = ('C', 0)
  f 'J' 'C' = ('C', y)
  f 'J' _ = ('J', 0)
