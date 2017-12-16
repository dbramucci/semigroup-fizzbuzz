import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

{-
-- | Allows fizz and buzz list to be easily made
mults :: Int -> String -> [Maybe String]
mults n text = cycle $ (replicate (n - 1) Nothing) ++ [Just text]

combine :: [Maybe String] -> [Maybe String] -> [String]
combine xs ys = zipWith fillInGaps words [1..]
    where words = zipWith (<>) xs ys
          fillInGaps maybeHole n = fromMaybe (show n) maybeHole


fizzbuzz = combine (mults 3 "fizz") (mults 5 "buzz")
              
main :: IO()
main = putStr . concat . map (++ "\n") $ take 100 (foldr1 (Just . combine) [mults 3 "fuzz", mults 4 "whats", mults 5 "bizz"])
-}

-- | Allows fizz and buzz list to be easily made
mults :: Int -> String -> [Maybe String]
mults n text = cycle $ (replicate (n - 1) Nothing) ++ [Just text]

combine :: [[Maybe String]] -> [String]
combine parts = zipWith fillInGaps words [1..]
    where words = foldr1 mergeLists parts
          mergeLists = zipWith (<>)
          fillInGaps maybeHole n = fromMaybe (show n) maybeHole

fizzbuzz :: [String]
fizzbuzz = combine [mults 3 "fizz", mults 5 "buzz"]
              
main :: IO()
main = putStr . concat . map (++ "\n") $ take 100 fizzbuzz


{-
fizzbuzz' = map f [1..]
    where f n = case (n `rem` 3, n `rem` 5) of
                    (0, 0) -> "fizzbuzz"
                    (_, 0) -> "buzz"
                    (0, _) -> "fizz"
                    _ -> show n-}

--import System.CPUTime
--import Control.DeepSeq
--import Criterion.Main
{-main = defaultMain [
    bgroup "semigroup fizzbuzz" (map (\n -> bench (show n) $ nf (\x -> take x fizzbuzz) n) [1,200000..1000000]),
    bgroup "normal fizzbuzz" (map (\n -> bench (show n) $ nf (\x -> take x fizzbuzz') n) [1,200000..1000000])]-}
    
    
{-map print (combine [fizzes, buzzes])
    where fizzes = mults 3 "fizz"
          buzzes = mults 5 "buzz"-}
          
    {-startTime <- getCPUTime
    {-foldr1 (>>) $ take 100000000 $ map putStr $ fizzbuzz-}
    endTime <- (take 100000000 fizzbuzz') `deepseq` getCPUTime
    putStrLn (show (fromIntegral (endTime - startTime) / 1000000000000))-}