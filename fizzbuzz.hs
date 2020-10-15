import Data.Maybe (fromMaybe)

-- | Allows fizz and buzz list to be easily made
mults :: Int -> String -> [Maybe String]
mults n text = cycle $ replicate (n - 1) Nothing ++ [Just text]

-- | Combines the _ _ fizz ... pattern with the _ _ _ _ buzz ... pattern.
-- specifically, if there are any strings, they get concatenated left to right.
-- If there are no strings, then the 1-based index is used.
combine :: foldable t => t [Maybe String] -> [String]
combine parts = zipWith (flip fromMaybe)
                    (foldr1 (zipWith (<>)) parts)
                  $ map show [1..]

fizzbuzz :: [String]
fizzbuzz = combine [mults 3 "fizz", mults 5 "buzz"]
              
main :: IO()
main = mapM_ print $ take 100 fizzbuzz
