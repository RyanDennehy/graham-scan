namespace GrahamScanTests

open NUnit.Framework

open GrahamScan.GrahamScan
open GrahamScan.Point
open GrahamScan.Util

open System

[<TestFixture>]
type GrahamScanTests () =
    static member private ScanError = "Convex hull was not successfully computed"

    [<TestCaseSource("ScanTestCases")>]
    static member ScanTest expected inPoints =
        let hull = GrahamScan inPoints
        CollectionAssert.AreEqual(expected, hull, GrahamScanTests.ScanError)

    static member private radiansToPoints =
        let f a = UnitCirclePoint (a * Math.PI)
        Array.map f >> List.ofArray
 
    static member ScanTestCases =
        [|
            [|
                [| (0.0, 0.0); (2.0, 0.0); (2.0, 2.0); (0.0, 2.0) |] |> CoordsToPoint;
                [| (0.0, 0.0); (2.0, 0.0); (2.0, 2.0); (0.0, 2.0) |] |> CoordsToPoint
            |];
            [|
                [| (0.0, 0.0); (3.0, 1.0); (5.0, 3.0); (4.0, 4.5); (1.0, 4.0) |] |> CoordsToPoint;
                [| (0.0, 0.0); (1.0, 4.0); (5.0, 3.0); (3.0, 1.0); (2.0, 2.0); (4.0, 4.5) |] |> CoordsToPoint
            |];
            [|
                [| 0.0; 1.0 / 6.0; 1.0 / 3.0; 0.5; 2.0 / 3.0 |] |> GrahamScanTests.radiansToPoints;
                [| 0.0; 1.0 / 6.0; 1.0 / 3.0; 0.5; 2.0 / 3.0 |] |> GrahamScanTests.radiansToPoints
            |];
            [|
                [| (0.0, 0.0); (5.0, 3.0); (2.0, 4.0); (1.0, 4.0) |] |> CoordsToPoint;
                [| (0.0, 0.0); (1.0, 2.0); (5.0, 3.0); (2.0, 4.0); (1.0, 4.0) |] |> CoordsToPoint
            |];
            [|
                [| (1.0, 1.0); (5.0, 1.0); (13.0, 5.0); (1.0, 9.0); (-2.0, 4.0) |] |> CoordsToPoint;
                [| (1.0, 1.0); (5.0, 1.0); (4.0, 7.0); (13.0, 5.0); (1.0, 9.0); (-2.0, 4.0) |] |> CoordsToPoint
            |]
        |]