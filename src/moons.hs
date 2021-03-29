import Control.Monad

main = readLn >>= \t -> forM_ [1..t] $ \t' ->
  getParams >>= putStrLn . cs t' . show . solve where
  cs t xs = "Case #" ++ show t ++ ": " ++ xs

getParams = do
  [x, y, s] <- words <$> getLine
  return (read x, read y, s) :: IO (Int, Int, String)

-- Infinite trinary tree representing all possible strings on the alphabet "CJ?"
data CJQ a = CJQ a (CJQ a) (CJQ a) (CJQ a)

applyCJQ :: (String -> a) -> CJQ a
applyCJQ f = go "" where
  go s = s `seq` CJQ (f s) (go $ 'C':s) (go $ 'J':s) (go $ '?':s)

index :: CJQ a -> String -> a
index (CJQ v l m r) [] = v
index (CJQ v l m r) ('C':s) = index l s
index (CJQ v l m r) ('J':s) = index m s
index (CJQ v l m r) ('?':s) = index r s

solve :: (Int, Int, String) -> Int
solve (x, y, s) = cost s where
  costs = applyCJQ (f cost)
  cost = index costs . reverse
  f mf ('?':s) = min (mf ('J':s)) (mf ('C':s))
  f mf (a:'?':s) = min (mf (a:'J':s)) (mf (a:'C':s))
  f mf ('C':'J':s) = x + mf ('J':s)
  f mf ('J':'C':s) = y + mf ('C':s)
  f mf (a:a':s) = mf (a:s)
  f _ _ = 0
