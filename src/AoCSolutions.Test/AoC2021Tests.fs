namespace AoCSolutions.Test

module AoC2021Tests =

    open AoCSolutions.E2021
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    /// Day 01:
    let day01InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/day/01/2021-01.input"
          )

    let testDepths =
      [
        "199"; "200"; "208"; "210"; "200"; "207";
        "240"; "269"; "260"; "263"
      ] |> List.toSeq
    
    [<Test>]
    let TestSimpleIncreasingDepthsComputation () =
      let c =
        testDepths
        |> Seq.map (fun line -> (int line))
        |> day01.computeIncreasingDepthsCount
      Assert.That(c, Is.EqualTo 7)
    
    [<Test>]
    let TestDay01Puzzle1 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let depthsCount = day01.solvePuzzle01 day01InputFile01
      Assert.That(depthsCount, Is.EqualTo 1475)

    [<Test>]
    let TestWindowSumComputation () =
      let testWindow = [0; 1; 2]
      let sum1 =
        testDepths
        |> Seq.map (fun line -> (int line))
        |> day01.sumUpWindow testWindow
      Assert.That(sum1, Is.EqualTo 607)

    [<Test>]
    let TestWindowedIncreasingDepthsComputation () =
      let c =
        testDepths
        |> Seq.map (fun line -> (int line))
        |> day01.computeWindowedIncreasingDepthsCount
      Assert.That(c, Is.EqualTo 5)

    [<Test>]
    let TestDay01Puzzle2 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let depthsCount = day01.solvePuzzle02 day01InputFile01
      Assert.That(depthsCount, Is.EqualTo 1516)


    /// Day 02:
    let day02InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/day/02/2021-02.input"
          )
    
    let testCourse =
      [
        "forward 5";
        "down 5";
        "forward 8";
        "up 3";
        "down 8";
        "forward 2"
      ]

    [<Test>]
    let TestDirDistanceParsing () =
      match day02.toDirectionAndDistance "up 4" with
      | day02.Direction.Up dist -> Assert.That(dist, Is.EqualTo 4)
      | _ -> Assert.Fail("Wrong direction!")
      match day02.toDirectionAndDistance "down 5" with
      | day02.Direction.Down dist -> Assert.That(dist, Is.EqualTo 5)
      | _ -> Assert.Fail("Wrong direction!")
      match day02.toDirectionAndDistance "forward 45" with
      | day02.Direction.Forward dist -> Assert.That(dist, Is.EqualTo 45)
      | _ -> Assert.Fail("Wrong direction!")

    [<Test>]
    let TestSubmPosComputation () =
      let (h, d) =
        testCourse
        |> List.toSeq
        |> day02.computePosition
      Assert.That(h, Is.EqualTo 15)
      Assert.That(d, Is.EqualTo 10)

    [<Test>]
    let TestDay02Puzzle1 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let hposTimesDepth = day02.solvePuzzle01 day02InputFile01
      Assert.That(hposTimesDepth, Is.EqualTo 1947824)

    [<Test>]
    let TestSubmPosComputationWithAim () =
      let (h, d) =
        testCourse
        |> List.toSeq
        |> day02.computePositionUsingAim
      Assert.That(h, Is.EqualTo 15)
      Assert.That(d, Is.EqualTo 60)

    [<Test>]
    let TestDay02Puzzle2 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let hposTimesDepth = day02.solvePuzzle02 day02InputFile01
      Assert.That(hposTimesDepth, Is.EqualTo 1813062561)
