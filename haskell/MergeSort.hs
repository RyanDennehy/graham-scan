module MergeSort where

halve :: [a] -> ([a], [a])
halve xs = let h = (length xs) `div` 2
           in ((take h xs), (drop h xs))

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort f xs = 
    if (length xs) < 2 then xs
    else
        let (left, right) = halve xs
            resL = mergeSort f left
            resR = mergeSort f right
        in merge f resL resR

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f as bs =
    case (as, bs) of
        (_, []) -> as
        ([], _) -> bs
        ((ah:at), (bh:bt)) -> if (f ah bh) 
                             then ah:(merge f at bs)
                             else bh:(merge f as bt)

mergeWrapper :: Ord a => [a] -> [a]
mergeWrapper xs = mergeSort (<=) xs
