import Control.Monad
import Data.List (unfoldr)

main = readLn >>= \t -> forM_ [1..t] $ \t' ->
  getParams >>= putStrLn . cs t' . disp . solve where
  cs t xs = "Case #" ++ show t ++ ": " ++ xs
  disp Nothing = "IMPOSSIBLE"
  disp (Just xs) = unwords $ map show xs

getParams = (\[n, c] -> (n, c)) . map read . words <$> getLine :: IO (Int, Int)

solve (n, c) = produce 1 <$> partition (n, c)

partition (n, c)
  | c < n - 1 || c >= n * (n + 1) `div` 2 = Nothing
  | otherwise = Just $ unfoldr f (n, c) where
    f (1, _) = Nothing
    f (m, d) = let c' = min (d - m + 2) m in 
                    Just (c', (m - 1, d - c'))

produce n [] = [n]
produce n (i:is) = let (xs, xs') = splitAt (i - 1) $ produce (n + 1) is in
                       reverse xs ++ n : xs'

verify = go 1 where
  go _ [x] = 0
  go n xs = let (xs', _:xs'') = span (/= n) xs in
                1 + length xs' + go (n + 1) (reverse xs' ++ xs'')
