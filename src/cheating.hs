import Control.Monad
import Data.List (maximumBy)
import Data.Ord (comparing)

main = readLn >>= \t -> getLine >>
  forM_ [1..t] (\t' ->
    replicateM 100 getLine >>= putStrLn . cs t' . show . solve) where
  cs t xs = "Case #" ++ show t ++ ": " ++ xs

sums :: [[Char]] -> [Int]
sums = foldl (zipWith g) (repeat 0) where
  g n '1' = n + 1
  g n '0' = n

solve lines = fst $ maximumBy (comparing $ cheat . snd) $ zip [1..] lines where
  cheat xs = sum $ zipWith f (sums lines) xs where
    strength = fromIntegral $ length $ filter (== '1') xs
    f d '1' = (15000 - strength) / fromIntegral d ** 4.0
    f d '0' = 0.0
