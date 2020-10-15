{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))

-- | Zips together lists of monoids using mappend
monoidZipAll :: (Foldable t, Monoid a) => t [a] -> [a]
monoidZipAll = foldr (zipWith mappend) (repeat mempty)


-- | Allows fizz and buzz list to be easily made
mults :: Int -> a -> [Maybe a]
mults n text = cycle $ replicate (n - 1) Nothing ++ [Just text]


-- | Combines the _ _ fizz ... pattern with the _ _ _ _ buzz ... pattern.
-- specifically, if there are any strings, they get concatenated left to right.
-- If there are no strings, then the 1-based index is used.
combine :: (IsString s, Semigroup s, Foldable t) => t [Maybe s] -> [s]
combine = combine' id

-- | Like `combine`, but it allows you to process the strings before zipping with numbers.
combine' :: (Foldable t, Semigroup a, IsString s) => (a -> s) -> t [Maybe a] -> [s]
combine' postProcess parts = zipWith (flip fromMaybe)
                    (fmap postProcess <$> monoidZipAll parts)
                  $ map (fromString . show) [1..]


fizzbuzz :: [String]
fizzbuzz = combine [mults 3 "fizz", mults 5 "buzz"]


main :: IO()
main = mapM_ putStrLn $ take 100 fizzbuzz

-- Demonstration of how to get "Fizz Buzz!" to be the displayed behavior.


newtype WordedString = WordedString {getString :: String}


instance IsString WordedString where
    fromString = WordedString


instance Semigroup WordedString where
    (WordedString x) <> (WordedString y) = WordedString $ x ++ " " ++ y


fizzbuzz' :: [String]
fizzbuzz' = combine' ((++"!") . getString) [mults 3 "Fizz", mults 5 "Buzz"]
