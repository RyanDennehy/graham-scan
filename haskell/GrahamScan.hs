module GrahamScan where

import PointData

-- doublesToPoints
-- Turns a list of doubles into a list of points
doublesToPoints :: [Double] -> [Point2D]
doublesToPoints (x:y:zs) = [Point x y] ++ doublesToPoints zs
doublesToPoints _        = []

-- toPoints
-- Turns a string of doubles into a list of points
toPoints :: String -> [Point2D]
toPoints = doublesToPoints . map (\r -> read r :: Double) . words

-- notStart
-- Creates a list of points that are not equal to the starting point
notStart :: Point2D -> [Point2D] -> [Point2D]
notStart start = filter (\p -> not (p == start))

-- doScan
-- Perform the Graham Scan
doScan :: [Point2D] -> [Point2D] -> [Point2D]
doScan ps@(x:xs) stack =
    case stack of (p1:p2:_) -> if ((length stack) > 1) && ((lineDirection p2 p1 x) == RightTurn)
                               then doScan ps (tail stack)
                               else doScan xs (x:stack)
                  otherwise -> doScan xs (x:stack)
doScan _ stack = stack

-- grahamScan
-- Performs the Graham Scan algorithm
grahamScan :: [Point2D] -> [Point2D]
grahamScan ps = let startPoint  = (startingPoint ps)
                    otherPoints = sortPoints startPoint (notStart startPoint ps)
                in reverse (doScan (startPoint:otherPoints) [])
