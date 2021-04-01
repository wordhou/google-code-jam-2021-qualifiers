import Control.Monad

main = readLn >>= \t -> forM_ [1..t] $ \t' ->
  getParams >>= putStrLn . cs t' . disp . solve where
  cs t xs = "Case #" ++ show t ++ ": " ++ xs
  disp = show

getParams = getLine >> map read . words <$> getLine :: IO [Int]

solve :: [Int] -> Int
solve = go 1 where
  go :: Int -> [Int] -> Int
  go _ [] = 0
  go n xs = let (xs', _:xs'') = span (/= n) xs in
                1 + length xs' + go (n + 1) (reverse xs' ++ xs'')
