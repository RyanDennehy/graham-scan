module MergeSort where

-- halve
-- Split a list into two lists. If the length of the list is even, the result
-- lists will be of equal length; if the length of the list is odd, the second
-- result list will have one more element
halve :: [a] -> ([a], [a])
halve xs = let h = (length xs) `div` 2
           in ((take h xs), (drop h xs))

-- mergeSort
-- Implementation of Merge Sort
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort f xs = 
    if (length xs) < 2 then xs
    else
        let (left, right) = halve xs
            resL = mergeSort f left
            resR = mergeSort f right
        in merge f resL resR

-- merge
-- Merge two lists into one sorted list
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f xs ys =
    case (xs, ys) of
        (_, []) -> xs
        ([], _) -> ys
        ((a:at), (bh:bt)) -> if (f a bh) 
                             then a:(merge f at ys)
                             else bh:(merge f xs bt)

-- mergeWrapper
-- Handy function for the usual case of sorting for any Ord type
mergeWrapper :: Ord a => [a] -> [a]
mergeWrapper = mergeSort (<=)
