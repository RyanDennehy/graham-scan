namespace GrahamScan

open Point

module Util =
    let public CoordsToPoint =
        let f = fun (x, y) -> Point(x, y)
        Array.map f >> List.ofArray

    let public UnitCirclePoint a =
        Point(cos a, sin a)