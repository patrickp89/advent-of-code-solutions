namespace AoCSolutions.E2018.Test

module Day01Test =

    open AoCSolutions.E2018.Day01
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    [<Test>]
    let Day01Test1 () =
        let f =
            Path.Combine(
                TestContext.CurrentContext.TestDirectory,
                "../../../",
                @"2018/inputs/day/01/2018-01.input"
        )
        let lines = File.ReadLines f
        Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
        Assert.That(solvePuzzle01 f, Is.EqualTo 582)
