namespace GrahamScanTests

open NUnit.Framework

open GrahamScan.GrahamScan
open GrahamScan.Point

open System

[<TestFixture>]
type PointTests () =
    static member private StartPoint = Point(0.0, 0.0)
    static member private Epsilon = 10e-6

    static member private DistanceError = "Distance was not calculated correctly"
    static member private StartError = "Did not find the correct start point"
    static member private PolarError = "Polar angle was not calculated correctly"
    static member private AnglesNotEqual = "Points did not have the same angle relative to the start point"
    static member private DedupError = "Did not deduplicate list correctly"
    static member private SortError = "Points were not sorted correctly"

    [<TestCaseSource("DistanceCases")>]
    static member DistanceTest x1 y1 x2 y2 expected =
        let p1 = Point(x1, y1)
        let p2 = Point(x2, y2)
        let actual = Point.Distance p1 p2
        Assert.AreEqual(expected, actual, PointTests.DistanceError)

    static member DistanceCases =
        [|
            [| 1.0; 1.0; 1.0; 1.0; 0.0 |];
            [| 3.0; 0.0; 0.0; 4.0; 5.0 |]
        |]

    [<TestCaseSource("StartingPointCases")>]
    static member StartingPointTest expected ps =
        let actual = StartingPoint ps
        Assert.AreEqual(expected, actual, PointTests.StartError)

    static member StartingPointCases: obj [] [] =
        [|
            [| PointTests.StartPoint; [ PointTests.StartPoint ] |] ;
            [| Point(1.0, 1.0); [ Point(2.0, 2.0); Point(2.0, 1.0); Point(1.0, 2.0); Point(1.0, 1.0) ] |]
        |]

    [<TestCaseSource("PolarAngleCases")>]
    static member PolarAngleTest expected p =
        let actual = Point.AngleBetween PointTests.StartPoint p
        Assert.AreEqual(expected, actual, PointTests.Epsilon, PointTests.PolarError)

    static member PolarAngleCases: obj [] [] =
        [|
            [| 0.0; Point(1.0, 0.0) |];
            [| Math.PI / 6.0; Point(0.5 * sqrt 3.0, 0.5) |];
            [| Math.PI / 3.0; Point(0.5, 0.5 * sqrt 3.0) |];
            [| Math.PI / 2.0; Point(0.0, 1.0) |];
            [| Math.PI * 2.0 / 3.0; Point(-0.5, 0.5 * sqrt 3.0) |];
            [| Math.PI * 5.0 / 6.0; Point(-0.5 * sqrt 3.0, 0.5) |];
            [| Math.PI; Point(-1.0, 0.0) |]
        |]

    [<TestCaseSource("PolarInLineCases")>]
    static member PolarInLineTest p1 p2 =
        let a1 = Point.AngleBetween PointTests.StartPoint p1
        let a2 = Point.AngleBetween PointTests.StartPoint p2
        Assert.AreEqual(a1, a2, PointTests.Epsilon, PointTests.AnglesNotEqual)

    static member PolarInLineCases =
        [|
            [| Point(0.0, 5.0); Point(0.0, 10.0) |];
            [| Point(1.0, 2.0); Point(2.0, 4.0) |]
        |]

    static member CoordsToPoint =
        let f = fun (x, y) -> Point(x, y)
        Array.map f >> List.ofArray

    [<TestCaseSource("DedupByEqCases")>]
    static member DedupByEqTest expecteds actuals =
        let expected = PointTests.CoordsToPoint expecteds
        let actual   = PointTests.CoordsToPoint actuals |> DedupByEq
        CollectionAssert.AreEqual(expected, actual, PointTests.DedupError)

    static member DedupByEqCases =
        [|
            [|
                [| (1.0, 2.0); (1.0, 1.0) |];
                [| (1.0, 2.0); (1.0, 1.0) |]
            |];
            [|
                [| (1.0, 1.0) |];
                [| (1.0, 1.0); (1.0, 1.0) |]
            |];
            [|
                [| (1.0, 1.0) |];
                [| (1.0, 1.0); (1.0, 1.0); (1.0, 1.0) |]
            |];
            [|
                [| (1.0, 1.0); (2.0, 3.0) |];
                [| (1.0, 1.0); (2.0, 3.0); (2.0, 3.0); (1.0, 1.0) |]
            |];
        |]

    [<TestCaseSource("DedupByAngleCases")>]
    static member DedupByAngleTest expecteds actuals =
        let expected = PointTests.CoordsToPoint expecteds
        let actual   = PointTests.CoordsToPoint actuals |> DedupByAngle PointTests.StartPoint
        CollectionAssert.AreEqual(expected, actual, PointTests.DedupError)

    static member DedupByAngleCases =
        [|
            [|
                [| (1.0, 2.0); (1.0, 1.0) |];
                [| (1.0, 2.0); (1.0, 1.0) |]
            |];
            [|
                [| (1.0, 1.0) |];
                [| (1.0, 1.0); (1.0, 1.0) |]
            |];
            [|
                [| (10.0, 20.0) |];
                [| (1.0, 2.0); (10.0, 20.0) |]
            |];
            [|
                [| (100.0, 200.0) |];
                [| (1.0, 2.0); (10.0, 20.0); (100.0, 200.0) |]
            |];
            [|
                [| (2.0, 4.0); (3.0, 2.0) |];
                [| (1.0, 2.0); (2.0, 4.0); (3.0, 2.0); (1.5, 1.0) |]
            |];
        |]

    [<TestCaseSource("SortByAngleCases")>]
    static member SortByAngleTest expecteds actuals =
        let expected = PointTests.CoordsToPoint expecteds
        let actual   = PointTests.CoordsToPoint actuals |> SortPoints PointTests.StartPoint
        CollectionAssert.AreEqual(expected, actual, PointTests.SortError)

    static member SortByAngleCases =
        [|
            [|
                [||];
                [||]
            |];
            [|
                [| (10.0, 0.0); (10.0, 5.0); (10.0, 10.0); (5.0, 10.0); (0.0, 10.0) |];
                [| (10.0, 5.0); (0.0, 10.0); (10.0, 10.0); (10.0, 0.0); (5.0, 10.0) |]
            |];
            [|
                [| (5.0, 1.0); (4.0, 2.0); (8.0, 7.0); (6.0, 6.0); (2.0, 6.0); (1.0, 4.0) |];
                [| (5.0, 1.0); (3.0, 3.0); (1.0, 4.0); (2.0, 6.0); (8.0, 7.0); (6.0, 6.0); (4.0, 2.0) |]
            |]
        |]