namespace GrahamScan

open Point

module GrahamScan =
    let LowerLeftPoint (a:Point) (b: Point) =
        if a.Y < b.Y then a
        else if a.Y = b.Y && a.X < b.X then a
        else b

    let StartingPoint =
        List.reduce LowerLeftPoint

    let OrderByAngle p a b =
        let v = Point.AngleBetween p a - Point.AngleBetween p b
        if v < 0.0 then -1 else if v > 0.0 then 1 else 0

    let rec Dedup eq selector (points: Point list) =
        match points with
        | head :: tail when tail |> List.exists (eq head) ->
            let (yesmatch, nomatch) = List.partition (eq head) tail
            let selected = selector (head :: yesmatch)
            let rest = Dedup eq selector nomatch
            in selected :: rest
        | head :: tail -> head :: Dedup eq selector tail
        | _ -> points

    let DedupByEq =
        Dedup ( = ) List.head

    let DedupByAngle p =
        Dedup (Point.AngleEqual p) (Point.Furthest p)

    let DoDedup p ps = DedupByAngle p ps |> DedupByEq

    let SortPoints p (ps: Point list) = DoDedup p ps |> List.sortWith (OrderByAngle p)

    let rec DoublesToPoints points =
        match points with
        | x :: y :: tail -> Point(x, y) :: DoublesToPoints tail
        | _ -> []