namespace AoCSolutions.Test

module AoC2018Tests =

    open AoCSolutions.E2018.day01
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
                @"2018/day/01/2018-01.input"
        )
        let lines = File.ReadLines f
        Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
        Assert.That(solvePuzzle01 f, Is.EqualTo 582)
