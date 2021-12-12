namespace AoCSolutions.E2019.Test

module Day01Tests =

    open AoCSolutions.E2019.Day01
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
        ()

    let day01InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2019/inputs/day/01/2019-01.input"
      )

    [<Test>]
    let TestMassCalculation () =
      let mass = calculateMass 94735
      Assert.That(mass, Is.EqualTo 31576)
      
    [<Test>]
    let TestMassCalculationWithFuel () =
      let mass = calculateMassIncludingFuel 1969
      Assert.That(mass, Is.EqualTo 966)

    [<Test>]
    let TestDay01Puzzle1 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let mass = solvePuzzle01 day01InputFile01
      Assert.That(mass, Is.EqualTo 3394106)

    [<Test>]
    let TestDay01Puzzle2 () =
      let lines = File.ReadLines day01InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let mass = solvePuzzle02 day01InputFile01
      Assert.That(mass, Is.EqualTo 5088280)
