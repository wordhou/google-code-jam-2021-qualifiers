# reversort

We're being asked to determine the cost of running the "reverse sort" algorithm on a given list of integers from `[1..n]`. The reverse sort algorithm proceeds as follows. Note that in the following pseudo-code the array is indexed starting from 1:

```
for each i = 1 .. n - 1
  j <- the index of the minimum element with j >= i
  Reverse the sub-array between indices [i:j], inclusive
```

Each step of this algorithm puts the number `i` at the `i`th position. The cost of the algorithm is the size of the sub-array reversed in each step. Note that if the `i`th element is already in the `i`th position, the algorithm still reverses the sub-array that contains `i`, costing 1.

Since the input sizes were small (n < 100), running the algorithm directly gave the simplest solution.

```haskell
solve :: [Int] -> Int
solve = go 1 where
  go _ [x] = 0
  go n xs = let (xs', _:xs'') = span (/= n) xs in
                1 + length xs' + go (n + 1) (reverse xs' ++ xs'')
```

# moons

The problem asks us to find the optimal cost on a family of binary strings, where the cost grows linearly with the number of times the string changes values. We're given integers X, and Y, where X is the cost of changing from `C` to `J`, and Y is the cost of changing from `J` to `C`. Given the costs X and Y and a string `C?J??J?C` with some blank values, what's the optimal cost?

The simple version of the problem involves strictly non-negative costs. Since we can minimize the cost by fill in the blanks with the non-blank value immediately to the left or the right of the blank, we just need to count the number of times the input string changes values:

```haskell
solve :: (Int, Int, String) -> Int
solve (x, y, s) = sum . snd $ mapAccumL f (head s) s where
  f '?' a = (a, 0)
  f 'C' 'J' = ('J', x)
  f 'C' _ = ('C', 0)
  f 'J' 'C' = ('C', y)
  f 'J' _ = ('J', 0)
```

# engineering

In Reversort Engineering, we're asked to produce a list of integers from `[1..n]` for which the cost of sorting it via Reversort is exactly `c`.

At each step `i` in `[1..n-1]` in the reversort algorithm, we're reversing a list of size N<sub>i</sub>. The first insight is that this value N<sub>i</sub> ranges from [1..i]. The second insight is that the knowing the values of N<sub>i</sub> for a given list uniquely determine the order of the list. A quick sanity check tells us that since the N<sub>i</sub> have `n`, `n-1`, `n-2`, … , `2` possible values, there are exactly n! values for the collection of N<sub>i</sub>, which is equal to the number of possible lists.

Given these two facts, we just need to determine if we can produce a list of integers that satisfies the constraints on our N<sub>i</sub> such that the sum of the N<sub>i</sub> is equal to the cost `c`. Then given those values, we can build the list by reversing the algorithm.

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

This interactive problem asked us to sort a list of distinct elements using a median-query operation instead of a comparison operation. The median-query operation tells us, given three elements (A, B, C) which of those elements is the median of the three. The problem parameters were such that we needed to sort each list of 50 distinct elements in less than 170 queries on average.

Of course using the median operation, we can never distinguish a list from its reverse (a list where the elements are in reverse order), so the problem accepts a list or the reverse list. Without loss of generality we assume that the first element is less than the second element in the list. This assumption unique determines the sorted list.

The strategy is as follows. Given two elements A and B for which we assume `A < B`, we perform a median-query on every set of (A, B, X) where X ranges from the rest of the elements. This allows us to place every other element into three bins: the set of elements strictly less than A, the set of elements between A and B, and the set of elements greater than B.

If a bin has either zero or one elements, then it is already sorted. If a bin has more than one element, we can recursively sort it with the following strategy. Choose two elements A' and B' arbitrarily from the bin. We perform a median-query on (A', B', A), which should tell us either A' < B' or B' < A', since we knowing which bin we're in tells us how A' and B' compare to A. Since we now have two elements with known order inside the bin, we can recursively sort the bin using this algorithm.

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

In this problem there are 100 competitors to a trivia competition with 10000 questions. Each competitor can answer a question either correctly or incorrectly. The probability of a competitor answering a question is based on two hidden variables: the strength of the competitor, and the difficulty of the question, which are both uniform on the interval [-3, 3]. The probability distribution is the sigmoid or logistic distribution based on the difference between the competitor's strength and the questions difficulty.

Not having an especially strong background in statistics, I took a look at the shape of the logistic curve. This plot from Wikipedia ranges from [-6, 6], which conveniently is the range that's relevant to this problem:

![Logistic Curve plot from Wikipedia](./Logistic-curve.svg)

The difference for any given competitor-question pair ranges from -6 to 6 but is [concentrated in the middle](https://en.wikipedia.org/wiki/Triangular_distribution). However, we can infer from the graph that when a competitor has strength close to -3 and a problem has difficulty close to 3, there's almost zero change that the competitor can get the problem right. Even supposing a competitor had average strength, on the most difficult problems it looks like they have less than a 5% chance to get the problem right.

This gives us a basis on which to detect a cheater. I rank each competitor by a "cheat-indicator" function which sums the number of times a competitor gets a question right, weighted the difficulty of that question. Since I don't know enough statistics to be able to estimate the difficulty of a question from the data given, I'm using the number of times a question was answered correctly as a proxy for the difficulty of the question. To be specific, I took the inverse fourth power of the number of times a question was answered correctly. The cheat indicator was also weighted inversely by the estimated strength of the competitor, which was estimated by the number of questions they answered correctly. These parameters were arrived at experimentally, choosing values that gave the most separation between the known cheater and second-most-suspicious competitor in the cheat metric.

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
