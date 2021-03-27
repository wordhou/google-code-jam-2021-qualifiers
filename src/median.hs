import Control.Monad
import System.IO (hFlush, stdout)
import Data.Tuple (swap)

main = getLine >>= (\[t, n, q] -> replicateM_ t $ run n) . map read . words

run n = medianSort [3..n] (1,2) >>= putStrLn . unwords . map show >>
  hFlush stdout >> getLine >> return ()

query (x, y, z) = putStrLn (unwords $ map show [x, y, z]) >>
  hFlush stdout >> readLn
  
sortPairAbove low a b = g <$> query (low, a, b) where
  g x | x == a = (a, b)
      | x == b = (b, a)

sortPairBelow high a b = swap <$> sortPairAbove high a b

medianSort :: [Int] -> (Int, Int) -> IO [Int]
medianSort [] (a, b) = return [a, b]
medianSort ns (a, b) = do
  (xs, ys, zs) <- bin (a, b) ns
  xs' <- case xs of
           (a':b':xs'') -> sortPairBelow a a' b' >>= medianSort xs''
           xs'' -> return xs''
  ys' <- case ys of
           (a':b':ys'') -> sortPairAbove a a' b' >>= medianSort ys''
           ys'' -> return ys''
  zs' <- case zs of
           (a':b':zs'') -> sortPairAbove b a' b' >>= medianSort zs''
           zs'' -> return zs''
  return $ xs' ++ a : ys' ++ b : zs'

bin :: (Int, Int) -> [Int] -> IO ([Int], [Int], [Int])
bin (a, b) = foldM f ([], [], []) where
  f (xs, ys, zs) n = g <$> query (a, b, n) where
    g m | m == a = (n:xs, ys, zs)
        | m == b = (xs, ys, n:zs)
        | m == n = (xs, n:ys, zs)
