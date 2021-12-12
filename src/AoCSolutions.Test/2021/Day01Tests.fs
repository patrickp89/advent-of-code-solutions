namespace AoCSolutions.E2021.Test

module Day01Tests =

    open AoCSolutions.E2021.Day01
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    let day01InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/inputs/day/01/2021-01.input"
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
        |> computeIncreasingDepthsCount
      Assert.That(c, Is.EqualTo 7)
    
    [<Test>]
    let TestDay01Puzzle1 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let depthsCount = solvePuzzle01 day01InputFile01
      Assert.That(depthsCount, Is.EqualTo 1475)

    [<Test>]
    let TestWindowSumComputation () =
      let testWindow = [0; 1; 2]
      let sum1 =
        testDepths
        |> Seq.map (fun line -> (int line))
        |> sumUpWindow testWindow
      Assert.That(sum1, Is.EqualTo 607)

    [<Test>]
    let TestWindowedIncreasingDepthsComputation () =
      let c =
        testDepths
        |> Seq.map (fun line -> (int line))
        |> computeWindowedIncreasingDepthsCount
      Assert.That(c, Is.EqualTo 5)

    [<Test>]
    let TestDay01Puzzle2 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let depthsCount = solvePuzzle02 day01InputFile01
      Assert.That(depthsCount, Is.EqualTo 1516)
