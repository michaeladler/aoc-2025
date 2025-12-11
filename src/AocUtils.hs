module AocUtils where

import Data.List (sortBy, tails)
import Data.Ord (Down (..), comparing)

-- | Generate all unique unordered pairs from a list.
-- Each pair (x, y) consists of two distinct elements from the input list,
-- with each combination appearing only once (order does not matter).
choose2 :: [a] -> [(a, a)]
choose2 xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (comparing Down)
