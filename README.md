Here's a collection of solutions and write-ups to Google's 2021 Code Jam qualifier round, written in Haskell.

# Usage

To build all problems:

```
stack build
```

To run one problem:

```
stack run [problemName]
```

To load a problem into the GHCI repl:

```
stack repl 2021q:exe:[problemName]
```

# reversort

We're being asked to determine the cost of running the "reverse sort" algorithm on a given list of integers from `[1..n]`. The reverse sort algorithm proceeds as follows. (Note that in the following pseudo-code the array is indexed starting from 1.)

```
for each i = 1 .. n - 1
  j <- the index of the minimum element with j >= i
  Reverse the sub-array between indices [i:j], inclusive
```

Each step of this algorithm puts the number `i` at the `i`th position. The cost of the algorithm is the size of the sub-array reversed in each step. Note that if the `i`th element is already in the `i`th position, the algorithm still reverses a sub-array of length 1. The simple solution is just to perform the algorithm while summing the costs:

```haskell
solve :: [Int] -> Int
solve = go 1 where
  go _ [x] = 0
  go n xs = let (xs', _:xs'') = span (/= n) xs in
                1 + length xs' + go (n + 1) (reverse xs' ++ xs'')
```

# moons

he problem asks us to find the optimal cost over a family of binary strings, where the cost is based on the number of time the string changes values. We're given integers X and Y, where X is the cost of changing from `C` to `J`, and Y is the cost of changing from `J` to `C`. Given the costs X and Y and a string (such as `C?J??J?C`) with some blank values, what's the optimal cost of a string we can produce by filling in the blanks?

The simple version of the problem gives us strictly non-negative costs. Since we can minimize the cost by filling in the blanks with the first non-blank value to the left or the right of the blank, we just need to count the number of times the input string changes values:

```haskell
solve :: (Int, Int, String) -> Int
solve (x, y, s) = sum . snd $ mapAccumL f (head s) s where
  f '?' a = (a, 0)
  f 'C' 'J' = ('J', x)
  f 'C' _ = ('C', 0)
  f 'J' 'C' = ('C', y)
  f 'J' _ = ('J', 0)
```

The extra-credit version of the problem involves values of X and Y that can be negative. I did not come up with this solution during the competition but I've implemented this dynamic programming solution using a pure functional lazy tree/trie structure to memoize the solutions:

```haskell
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
```

# engineering

In Reversort Engineering, we're asked to produce a list of integers from `[1..n]` for which the cost of sorting it via the reversort algorithm is exactly `c`.

At each step `i` in `[1..n-1]` in the reversort algorithm, we're reversing a list of size N<sub>i</sub>. This value N<sub>i</sub> ranges from `[1 .. n + 1 - 1]`. Knowing the values of N<sub>i</sub> for a given list also uniquely determines the list. A quick proof of this fact: The N<sub>i</sub> have `n`, `n-1`, `n-2`, … , `2` possible values, respectively, so there are there are exactly n! values for the N<sub>i</sub>. There's a mapping from every valid set of N<sub>i</sub> onto every permutation of `[1..n]`. Since the lists both have size n!, the mapping must be a bijection.

Given these two facts, we just need to determine if we can produce a list of integers that satisfies the constraints on our N<sub>i</sub> that sum to `c`. Given those values, we can build the list by reversing the reversort algorithm.

```haskell
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
```

# median

This interactive problem asked us to sort a list of distinct elements using a _median-query operation_ instead of the usual comparison operation. The median-query operation tells us which of three elements (A, B, C) is the median.

Of course the median-query operation can never distinguish a list from its reverse, so the problem accepts either a list or its reverse. Without loss of generality, we assume that the first element is less than the second element in the list. This assumption uniquely determines the sorted list.

The strategy is as follows. Given two elements A and B for which we assume `A < B`, we perform a median-query on every (A, B, X) where X ranges from the rest of the elements. This allows us to place every other element into three bins: the set of elements strictly less than A, the set of elements between A and B, and the set of elements greater than B.

If a bin has either zero or one elements, then it is already sorted. If a bin has more than one element, we can recursively sort it with the following strategy. Choose two elements A' and B' arbitrarily from the bin. We perform a median-query on (A', B', A), which should tell us either A' < B' or B' < A'. Since we now have two elements with known order inside the bin, we can recursively sort the bin using this algorithm.

```haskell
query :: (Int, Int, Int) -> IO Int
query (x, y, z) = putStrLn (unwords $ map show [x, y, z]) >>
  hFlush stdout >> readLn

sortPairAbove :: Int -> Int -> Int -> IO (Int, Int)
sortPairAbove low a b = g <$> query (low, a, b) where
  g x | x == a = (a, b)
      | x == b = (b, a)

sortPairBelow :: Int -> Int -> Int -> IO (Int, Int)
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
```

# cheating

In this problem there are 100 competitors in a trivia competition with 10000 questions. Each competitor can answer a question either correctly or incorrectly. The probability of a competitor answering a question is based on two hidden variables: the strength of the competitor, and the difficulty of the question, which are both uniform distributed on the interval [-3, 3]. The probability distribution of answering the question correctly is the sigmoid distribution shifted by the difference between the competitor's strength and the questions difficulty.

Not having an especially strong background in statistics, I took a look at the shape of the logistic curve. This plot from Wikipedia ranges from [-6, 6], which conveniently is exactly the range that's relevant to this problem:

![Logistic Curve plot from Wikipedia](./Logistic-curve.svg)

The difference for any given competitor-question pair ranges from -6 to 6 but is [concentrated in the middle](https://en.wikipedia.org/wiki/Triangular_distribution). However, we can infer from the graph that when a competitor has an average strength, he or she has less than a 5% chance to answer the most difficult problems correctly.

This gives us a basis on which to detect a cheater. Rank each competitor by a "cheat-indicator" function which sums the number of times a competitor gets a question right, weighted the difficulty of that question. Since I don't know enough statistics to be able to estimate the hidden variables from the data given, I'm using the number of times a question was answered correctly as a proxy for the difficulty of the question. To be specific, I took the inverse fourth power of the number of times a question was answered correctly. The cheat indicator was also weighted inversely by the estimated strength of the competitor, which was estimated by the number of questions they answered correctly. The parameters were arrived at experimentally, choosing values that gave the most separation between the known cheater and second-most-suspicious competitor in the cheat metric.

```haskell
sums :: [[Char]] -> [Int]
sums = foldl (zipWith g) (repeat 0) where
  g n '1' = n + 1
  g n '0' = n

solve :: [[Char]] -> Int
solve lines = fst $ maximumBy (comparing $ cheat . snd) $ zip [1..] lines where
  cheat xs = sum $ zipWith f (sums lines) xs where
    strength = fromIntegral $ length $ filter (== '1') xs
    f d '1' = (15000 - strength) / fromIntegral d ** 4.0
    f d '0' = 0.0
```
