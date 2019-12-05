module PointData where

import Data.List (partition)
import MergeSort
import Utility

data Direction = LeftTurn
               | Straight
               | RightTurn
               deriving (Show, Eq)

data Point2D = Point { x :: Double, y :: Double } deriving (Eq)
instance Show Point2D where
    show p = "(" ++ show (x p) ++ ", " ++ show (y p) ++ ")"

-- pointsToVec
-- Calculates the vector between two points
pointsToVec :: Point2D -> Point2D -> Point2D
pointsToVec a b = (Point ((x b) - (x a)) ((y b) - (y a)))

-- wedgeProduct
-- Calculates the wedge product of two vectors
wedgeProduct :: Point2D -> Point2D -> Double
wedgeProduct u v = ((x u) * (y v)) - ((y u) * (x v))

-- lineDirection
-- Determines whether the path through three points turns left, right, or stays straight
lineDirection :: Point2D -> Point2D -> Point2D -> Direction
lineDirection a b c = let u = pointsToVec a b
                          v = pointsToVec b c
                          sign = wedgeProduct u v
                      in if sign > 0.0 then LeftTurn
                      else if sign < 0.0 then RightTurn
                      else Straight

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
-- Gets the polar angle of a point
polarAngle base height | height `approxEqual` 0.0 = if base > 0.0
                                                    then 0.0
                                                    else pi
                       | base   `approxEqual` 0.0 = if height > 0.0
                                                    then (pi / 2.0)
                                                    else (3.0 * pi / 2.0)
                       | base > 0.0 && height > 0.0 = atan (height / base)
                       | base < 0.0 = pi + (atan (height / base))
                       | otherwise = (2.0 * pi) + (atan (height / base))

-- polarAngleFromStart
-- Gets the polar angle between the starting point and another point
polarAngleFromStart :: Point2D -> Point2D -> Double
polarAngleFromStart start p2 =
    let base   = (x p2) - (x start)
        height = (y p2) - (y start)
    in polarAngle base height

-- dedup
-- Removes duplicate points from the input list
-- eq: Determines what it means for points to be equal
-- selector: Chooses which point to save from the duplicates
dedup :: Eq a => (a -> a -> Bool) -> ([a] -> a) -> [a] -> [a]
dedup _ _ [] = []
dedup eq selector (p:ps)
    | null ps = [p]
    | any (eq p) ps = let (match, nomatch) = partition (eq p) ps
                          selected = selector (p:match)
                          rest = dedup eq selector nomatch
                      in selected:rest
    | otherwise = p:(dedup eq selector ps)

-- orderByAngle
-- Ordering function to sort points by polar angle
orderByAngle :: Point2D -> Point2D -> Point2D -> Bool
orderByAngle start a b =
    let angle1 = polarAngleFromStart start a
        angle2 = polarAngleFromStart start b
    in angle1 < angle2

-- dedupByEq
-- Removes duplicate points from the list by strict equality
dedupByEq :: Eq a => [a] -> [a]
dedupByEq = dedup equality selector
    where equality p1 p2 = p1 == p2
          selector = head

-- angleEq
-- Gets whether the points make the same angle with the starting point
angleEq :: Point2D -> Point2D -> Point2D -> Bool
angleEq start a b = approxEqual (polarAngleFromStart start a) (polarAngleFromStart start b)

-- furthest
-- Gets the furthest points from the starting point
furthest :: Point2D -> [Point2D] -> Point2D
furthest start = foldl step start
    where step best p = if (dist start p) > (dist start best) then p else best

-- dedupByAngle
-- Finds duplicate points from the list by the angle they make with the
-- starting point
-- Keeps the point that is furthest from the starting point
dedupByAngle :: Point2D -> [Point2D] -> [Point2D]
dedupByAngle start = dedup equality selector
    where equality = angleEq start
          selector = furthest start

-- doDedup
-- Deduplicate by strict equality, then by polar angle
doDedup :: Point2D -> [Point2D] -> [Point2D]
doDedup start = dedupByAngle start . dedupByEq

-- sortPoints
-- Sort a list of points by the polar angle that they make with the starting point
sortPoints :: Point2D -> [Point2D] -> [Point2D]
sortPoints startPoint ps = mergeSort (orderByAngle startPoint) (doDedup startPoint ps)
