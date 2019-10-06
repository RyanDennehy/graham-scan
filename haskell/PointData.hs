module PointData where

import MergeSort
import Utility

data Direction = LeftTurn
               | Straight
               | RightTurn
               deriving (Show)

data Point2D = Point { x :: Double, y :: Double } deriving (Show, Eq)

-- slope
-- Calculates the slope between two points
slope :: Point2D -> Point2D -> Double
slope a b = ((y b) - (y a)) / ((x b) - (x a))

-- lineDirection
-- Determines whether the path through three points turns left, right, or stays straight
lineDirection :: Point2D -> Point2D -> Point2D -> Direction
lineDirection a b c | (slope a b) == (slope b c) = Straight
                    | (slope a b)  < (slope b c) = LeftTurn
                    | otherwise                  = RightTurn

-- turnsInSeries
-- Gets the series of turns that the points in a list make
turnsInSeries :: [Point2D] -> [Direction]
turnsInSeries (x:(xs@(y:z:_))) = (lineDirection x y z):(turnsInSeries xs)
turnsInSeries _ = []

-- startingPoint
-- Gets the starting point for the scan, the point in the "lower left"
startingPoint :: [Point2D] -> Point2D
startingPoint (p:[]) = p
startingPoint (p:ps) | (y p) < (y bestP) = p
                     | (y p) == (y bestP) && (x p) < (x bestP) = p
                     | otherwise = bestP
                     where bestP = (startingPoint ps)

-- dist
-- Gets the Euclidean distance between two points
dist :: Point2D -> Point2D -> Double
dist p1 p2 =
    let a2 = ((x p1) - (x p2)) ** 2
        b2 = ((y p1) - (y p2)) ** 2
        c2 = a2 + b2
    in sqrt c2

-- polarAngle
-- Gets the polar angle between the starting point and another point
-- by the Law of Cosines
polarAngle :: Point2D -> Point2D -> Double
polarAngle start p2 =
    let p3     = (Point ((x start) + 1.0) (y start))
        sideA  = 1.0
        sideB  = dist start p2
        sideC  = dist p2 p3
        frac   = ((sideA**2) + (sideB**2) - (sideC**2)) / (2 * sideA * sideB)
        angleC = acos frac
    in angleC

-- dedup
-- Removes duplicate points from the input list
-- eq: Determines what it means for points to be equal
-- selector: Chooses which point to save from the duplicates
dedup :: Eq a => (a -> a -> Bool) -> ([a] -> a) -> [a] -> [a]
dedup _ _ [] = []
dedup eq selector (p:ps)
    | null ps = [p]
    | any (eq p) ps = (selector (p:(filter (eq p) ps))):(dedup eq selector (filter (\x -> not(eq p x)) ps))
    | otherwise = p:(dedup eq selector ps)

-- orderByAngle
-- Ordering function to sort points by polar angle
orderByAngle :: Point2D -> Point2D -> Point2D -> Bool
orderByAngle start a b =
    let angle1 = polarAngle start a
        angle2 = polarAngle start b
    in angle1 < angle2

-- dedupByEq
-- Removes duplicate points from the list by strict equality
dedupByEq :: Eq a => [a] -> [a]
dedupByEq ps = dedup (\p q -> p == q) (\ps -> head ps) ps

-- angleEq
-- Gets whether the points make the same angle with the starting point
angleEq :: Point2D -> Point2D -> Point2D -> Bool
angleEq start a b = approxEqual (polarAngle start a) (polarAngle start b)

-- furthest
-- Gets the furthest points from the starting point
furthest :: Point2D -> [Point2D] -> Point2D
furthest start ps = foldl step start ps
    where step best p = if (dist start p) > (dist start best) then p else best

-- dedupByAngle
-- Finds duplicate points from the list by the angle they make with the
-- starting point
-- Keeps the point that is furthest from the starting point
dedupByAngle :: Point2D -> [Point2D] -> [Point2D]
dedupByAngle start ps = dedup (\p q -> angleEq start p q) (\xs -> furthest start xs) ps

-- doDedup
-- Deduplicate by strict equality, then by polar angle
doDedup :: Point2D -> [Point2D] -> [Point2D]
doDedup start ps = dedupByAngle start (dedupByEq ps)

-- sortPoints
-- Sort a list of points by the polar angle that they make with the starting point
sortPoints :: Point2D -> [Point2D] -> [Point2D]
sortPoints startPoint ps = mergeSort (orderByAngle startPoint) (doDedup startPoint ps)
