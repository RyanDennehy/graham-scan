namespace GrahamScan

open Direction
open System

module Point =
    [<StructuredFormatDisplay("Point ({x}, {y})")>]
    type Point(x: double, y: double) =
        member this.X = x
        member this.Y = y
        override this.ToString() = sprintf "(%f, %f)" x y

        override a.Equals(b) =
            match b with
            | :? Point as p -> (x, y) = (p.X, p.Y)
            | _ -> false

        override p.GetHashCode() =
            hash (x, y)

        /// <summary>
        /// Calculates the vector from one point to another
        /// </summary>
        /// <param name="a">The first point</param>
        /// <param name="b">The second point</param>
        /// <returns>The vector from the first point to the second point</returns>
        static member ToVec (a: Point) (b: Point) : Point =
            Point(b.X - a.X, b.Y - a.Y)

        /// <summary>
        /// Calculates the wedge product of two vectors
        /// </summary>
        /// <param name="u">The first vector</param>
        /// <param name="v">The second vector</param>
        /// <returns></returns>
        static member Wedge (u: Point) (v: Point) : double =
            (u.X * v.Y) - (u.Y * v.X)

        /// <summary>
        /// Determmines whether the path through three points turns left, right, or stays straight
        /// </summary>
        /// <param name="a">The first point</param>
        /// <param name="b">The second point</param>
        /// <param name="c">The third point</param>
        /// <returns>The <see cref="Direction"/> of the path through the three points</returns>
        static member LineBend a b c : Direction =
            let u = Point.ToVec a b
            let v = Point.ToVec b c
            let sign = Point.Wedge u v
            if sign > 0.0 then LeftTurn
            else if sign < 0.0 then RightTurn
            else Straight

        /// <summary>
        /// Gets the series of turns through a list of points
        /// </summary>
        /// <param name="points">The list of <see cref="Point">points</see></param>
        /// <returns>The series of turns through a list of points</returns>
        static member TurnsInSeries (points: Point list): Direction list =
            match points with
            | a :: (tail & b :: c :: _) -> Point.LineBend a b c :: Point.TurnsInSeries tail
            | _ -> []
           
        /// <summary>
        /// Gets the Euclidean distance between two points
        /// </summary>
        /// <param name="a">The first point</param>
        /// <param name="b">The second point</param>
        /// <returns>The Euclidean distance between two points</returns>
        static member public Distance (a: Point) (b: Point) =
            let dx2 = (a.X - b.X) ** 2.0
            let dy2 = (a.Y - b.Y) ** 2.0
            in sqrt (dx2 + dy2)

        /// <summary>
        /// 
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        static member PolarAngle x y =
            match (x, y) with
                | (_, 0.0) -> if x > 0.0
                              then 0.0
                              else Math.PI
                | (0.0, _) -> if y > 0.0
                              then Math.PI / 2.0
                              else Math.PI * 1.5
                | _ when x > 0.0 && y > 0.0 -> atan (y / x)
                | _ when x < 0.0 -> Math.PI + atan (y / x)
                | _ -> (2.0 * Math.PI) + atan (y / x)

        /// <summary>
        /// 
        /// </summary>
        /// <param name="p1"></param>
        /// <param name="p2"></param>
        /// <returns></returns>
        static member AngleBetween (p1: Point) (p2: Point) =
            let b = p2.X - p1.X
            let h = p2.Y - p1.Y
            in Point.PolarAngle b h

        /// <summary>
        /// 
        /// </summary>
        /// <param name="p"></param>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>
        static member AngleEqual p a b =
            Point.AngleBetween p a = Point.AngleBetween p b
            // ApproxEqual (Point.AngleBetween p a) (Point.AngleBetween p b)

        /// <summary>
        /// 
        /// </summary>
        /// <param name="p"></param>
        /// <param name="ps"></param>
        /// <returns></returns>
        static member Furthest (p: Point) (ps: Point list) =
            ps |> List.maxBy (Point.Distance p)