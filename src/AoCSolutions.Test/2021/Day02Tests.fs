namespace AoCSolutions.E2021.Test

module Day02Tests =

    open AoCSolutions.E2021.Day02
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    let day02InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/inputs/day/02/2021-02.input"
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
      match toDirectionAndDistance "up 4" with
      | Direction.Up dist -> Assert.That(dist, Is.EqualTo 4)
      | _ -> Assert.Fail("Wrong direction!")
      match toDirectionAndDistance "down 5" with
      | Direction.Down dist -> Assert.That(dist, Is.EqualTo 5)
      | _ -> Assert.Fail("Wrong direction!")
      match toDirectionAndDistance "forward 45" with
      | Direction.Forward dist -> Assert.That(dist, Is.EqualTo 45)
      | _ -> Assert.Fail("Wrong direction!")

    [<Test>]
    let TestSubmPosComputation () =
      let (h, d) =
        testCourse
        |> List.toSeq
        |> computePosition
      Assert.That(h, Is.EqualTo 15)
      Assert.That(d, Is.EqualTo 10)

    [<Test>]
    let TestDay02Puzzle1 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let hposTimesDepth = solvePuzzle01 day02InputFile01
      Assert.That(hposTimesDepth, Is.EqualTo 1947824)

    [<Test>]
    let TestSubmPosComputationWithAim () =
      let (h, d) =
        testCourse
        |> List.toSeq
        |> computePositionUsingAim
      Assert.That(h, Is.EqualTo 15)
      Assert.That(d, Is.EqualTo 60)

    [<Test>]
    let TestDay02Puzzle2 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let hposTimesDepth = solvePuzzle02 day02InputFile01
      Assert.That(hposTimesDepth, Is.EqualTo 1813062561)
