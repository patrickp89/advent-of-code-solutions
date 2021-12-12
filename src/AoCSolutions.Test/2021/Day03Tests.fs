namespace AoCSolutions.E2021.Test

module Day03Tests =

    open AoCSolutions.E2021.Day03
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    let day03InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/inputs/day/03/2021-03.input"
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
      let (zeroBitCount, oneBitCount) = testDiagnosticReport |> countBitsAtPos 0
      Assert.That(zeroBitCount, Is.EqualTo 5)
      Assert.That(oneBitCount, Is.EqualTo 7)
  
    [<Test>]
    let TestMcbCounting () =
      let mostCommonFirstBit = computeMostCommonBitAtPos testDiagnosticReport 0
      Assert.That(mostCommonFirstBit, Is.EqualTo 1)
      let mostCommonSecondBit = computeMostCommonBitAtPos testDiagnosticReport 1
      Assert.That(mostCommonSecondBit, Is.EqualTo 0)

    [<Test>]
    let TestGammaRateComputation () =
      let (binaryGammaRate, decimalGammaRate) = computeGammaRate testDiagnosticReport
      Assert.That(binaryGammaRate, Is.EqualTo "10110")
      Assert.That(decimalGammaRate, Is.EqualTo 22)

    [<Test>]
    let TestEpsilonRateComputation () =
      let (binaryGammaRate, _) = computeGammaRate testDiagnosticReport
      let (binaryEpsilonRate, decimalEpsilonRate) = computeEpsilonRate binaryGammaRate
      Assert.That(binaryEpsilonRate, Is.EqualTo "01001")
      Assert.That(decimalEpsilonRate, Is.EqualTo 9)

    [<Test>]
    let TestPowerConsumptionComputation () =
      let pc = computePowerConsumption testDiagnosticReport
      Assert.That(pc, Is.EqualTo 198)

    [<Test>]
    let TestDay03Puzzle1 () =
      let lines = File.ReadLines day03InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let pc = solvePuzzle01 day03InputFile01
      Assert.That(pc, Is.EqualTo 693486)

    [<Test>]
    let TestLifeSupportRatingDetermination () =
      let (oxygenGeneratorRating, co2ScrubberRating) =
        testDiagnosticReport
        |> determineLifeSupportRatings
      Assert.That(oxygenGeneratorRating, Is.EqualTo "10111")
      Assert.That(co2ScrubberRating, Is.EqualTo "01010")

    [<Test>]
    let TestLifeSupportRatingComputation () =
      let lsrtng = testDiagnosticReport |> computeLifeSupportRating
      Assert.That(lsrtng, Is.EqualTo 230)

    [<Test>]
    let TestDay03Puzzle2 () =
      let lines = File.ReadLines day03InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let lsrtng = solvePuzzle02 day03InputFile01
      Assert.That(lsrtng, Is.EqualTo 3379326)
