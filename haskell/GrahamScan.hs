module GrahamScan where

import PointData

-- doublesToPoints
-- Turns a list of doubles into a list of points
doublesToPoints :: [Double] -> [Point2D]
doublesToPoints ds =
    case ds of (x:y:zs)  -> [Point x y] ++ doublesToPoints zs
               otherwise -> []

-- toPoints
-- Turns a string of doubles into a list of points
toPoints :: String -> [Point2D]
toPoints ss = doublesToPoints (map (\r -> read r :: Double) (words ss))

-- notStart
-- Creates a list of points that are not equal to the starting point
notStart :: Point2D -> [Point2D] -> [Point2D]
notStart start ps = filter (\p -> not (p == start)) ps

-- doScan
-- Perform the Graham Scan
doScan :: [Point2D] -> [Point2D] -> [Point2D]
doScan [] stack = stack
doScan (x:xs) stack =
    case stack of (p1:p2:_) -> if ((length stack) > 1) && ((lineDirection p2 p1 x) == RightTurn)
                               then doScan (x:xs) (tail stack)
                               else doScan xs (x:stack)
                  otherwise -> doScan xs (x:stack)

-- grahamScan
-- Performs the Graham Scan algorithm
grahamScan :: [Point2D] -> [Point2D]
grahamScan xs = let startPoint = (startingPoint xs)
                    ps = sortPoints startPoint (notStart startPoint xs)
                in reverse (doScan (startPoint:ps) [])
