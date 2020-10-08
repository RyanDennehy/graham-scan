namespace GrahamScan

open Point

module GrahamScan =
    let private lowerLeftPoint (a:Point) (b: Point) =
        if a.Y < b.Y then a
        else if a.Y = b.Y && a.X < b.X then a
        else b

    let StartingPoint =
        List.reduce lowerLeftPoint

    let OrderByAngle p a b =
        let v = Point.AngleBetween p a - Point.AngleBetween p b
        if v < 0.0 then -1 else if v > 0.0 then 1 else 0

    let rec private dedup eq selector (points: Point list) =
        match points with
        | head :: tail when tail |> List.exists (eq head) ->
            let (yesmatch, nomatch) = List.partition (eq head) tail
            let selected = selector (head :: yesmatch)
            let rest = dedup eq selector nomatch
            in selected :: rest
        | head :: tail -> head :: dedup eq selector tail
        | _ -> points

    let DedupByEq =
        dedup ( = ) List.head

    let DedupByAngle p =
        dedup (Point.AngleEqual p) (Point.Furthest p)

    let DoDedup p = DedupByEq >> DedupByAngle p

    let SortPoints p ps = DoDedup p ps |> List.sortWith (OrderByAngle p)

    let rec DoublesToPoints points =
        match points with
        | x :: y :: tail -> Point(x, y) :: DoublesToPoints tail
        | _ -> []

    let private notStart start = List.filter (fun p -> p <> start)

    let rec private doScan points stack =
        match (points, stack) with
        | (p::ps, s1::s2::_) -> if stack.Length > 1 && (Point.LineBend s2 s1 p) = Direction.RightTurn
                                then doScan points stack.Tail
                                else doScan ps (p::stack)
        | (p::ps, _) -> doScan ps (p::stack)
        | (_, stack) -> stack
    
    let GrahamScan points =
        let startPoint = StartingPoint points
        let otherPoints = SortPoints startPoint (notStart startPoint points)
        List.rev (doScan (startPoint::otherPoints) [])