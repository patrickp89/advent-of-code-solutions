namespace AoCSolutions.Test

module AoC2021Tests =

    open AoCSolutions.E2021
    open FSharpx.Text.Strings
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


    /// Day 03:
    let day03InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/day/03/2021-03.input"
          )
    
    let testDiagnosticReport =
      [
        "00100";
        "11110";
        "10110";
        "10111";
        "10101";
        "01111";
        "00111";
        "11100";
        "10000";
        "11001";
        "00010";
        "01010"
      ]

    [<Test>]
    let TestBitCounting () =
      let (zeroBitCount, oneBitCount) = testDiagnosticReport |> day03.countBitsAtPos 0
      Assert.That(zeroBitCount, Is.EqualTo 5)
      Assert.That(oneBitCount, Is.EqualTo 7)
  
    [<Test>]
    let TestMcbCounting () =
      let mostCommonFirstBit = day03.computeMostCommonBitAtPos testDiagnosticReport 0
      Assert.That(mostCommonFirstBit, Is.EqualTo 1)
      let mostCommonSecondBit = day03.computeMostCommonBitAtPos testDiagnosticReport 1
      Assert.That(mostCommonSecondBit, Is.EqualTo 0)

    [<Test>]
    let TestGammaRateComputation () =
      let (binaryGammaRate, decimalGammaRate) = day03.computeGammaRate testDiagnosticReport
      Assert.That(binaryGammaRate, Is.EqualTo "10110")
      Assert.That(decimalGammaRate, Is.EqualTo 22)

    [<Test>]
    let TestEpsilonRateComputation () =
      let (binaryGammaRate, _) = day03.computeGammaRate testDiagnosticReport
      let (binaryEpsilonRate, decimalEpsilonRate) = day03.computeEpsilonRate binaryGammaRate
      Assert.That(binaryEpsilonRate, Is.EqualTo "01001")
      Assert.That(decimalEpsilonRate, Is.EqualTo 9)

    [<Test>]
    let TestPowerConsumptionComputation () =
      let pc = day03.computePowerConsumption testDiagnosticReport
      Assert.That(pc, Is.EqualTo 198)

    [<Test>]
    let TestDay03Puzzle1 () =
      let lines = File.ReadLines day03InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let pc = day03.solvePuzzle01 day03InputFile01
      Assert.That(pc, Is.EqualTo 693486)

    [<Test>]
    let TestLifeSupportRatingDetermination () =
      let (oxygenGeneratorRating, co2ScrubberRating) =
        testDiagnosticReport
        |> day03.determineLifeSupportRatings
      Assert.That(oxygenGeneratorRating, Is.EqualTo "10111")
      Assert.That(co2ScrubberRating, Is.EqualTo "01010")

    [<Test>]
    let TestLifeSupportRatingComputation () =
      let lsrtng = testDiagnosticReport |> day03.computeLifeSupportRating
      Assert.That(lsrtng, Is.EqualTo 230)

    [<Test>]
    let TestDay03Puzzle2 () =
      let lines = File.ReadLines day03InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let lsrtng = day03.solvePuzzle02 day03InputFile01
      Assert.That(lsrtng, Is.EqualTo 3379326)


    /// Day 04:
    let day04InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/day/04/2021-04.input"
          )
    
    let testBingoInput = 
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/day/04/2021-04.input.demo"
          )

    [<Test>]
    let TestBingoDrawsParsing () =
      let draws =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> day04.parseBingoDraws
      Assert.That(draws |> List.isEmpty, Is.EqualTo false)
      let draw1 = draws |> List.head
      Assert.That(draw1, Is.EqualTo 7)

    [<Test>]
    let TestBingoBoardsParsing () =
      let boardLines =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> day04.parseBingoBoardLines
      Assert.That(boardLines |> Seq.isEmpty, Is.EqualTo false)
      Assert.That(boardLines |> Seq.length, Is.EqualTo 3)
      let board1: string list = boardLines |> Seq.head
      Assert.That(board1 |> List.length, Is.EqualTo 5)
      let board1Line1: string = board1 |> List.head
      let expBoard1Line1 = "22 13 17 11  0"
      Assert.That(board1Line1, Is.EqualTo expBoard1Line1)
