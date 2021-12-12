namespace AoCSolutions.E2019.Test

module Day02Tests =

    open AoCSolutions.E2019.Day02
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
        ()

    let day02InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2019/inputs/day/02/2019-02.input"
      )

    [<Test>]
    let TestDay02Puzzle1 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let elf_prog_code = solvePuzzle01 day02InputFile01
      Assert.That(elf_prog_code, Is.EqualTo 2782414)

    [<Test>]
    let TestDay02Puzzle2 () =
      let lines = File.ReadLines day02InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let noun_and_verb = solvePuzzle02 day02InputFile01
      Assert.That(noun_and_verb, Is.EqualTo "98,20")
    
    [<Test>]
    let TestRangeCreation () =
      let testRange = range 1 5
      Assert.That(testRange, Is.EqualTo [1; 2; 3; 4; 5])
    
    [<Test>]
    let TestInDay02IntcodeProgComputation () =
      let testOpcode = compute_opcodes [1; 5; 6; 0; 99; 15; 10; 44] 0
      Assert.That(testOpcode, Is.EqualTo [25; 5; 6; 0; 99; 15; 10; 44])
      let testIntcodeProg1 = compute_intcode_program "1,9,10,3,2,3,11,0,99,30,40,50" 9 10
      Assert.That(testIntcodeProg1, Is.EqualTo 3500)
      let testIntcodeProg2 = compute_intcode_program "1,1,1,4,99,5,6,0,99" 1 1
      Assert.That(testIntcodeProg2, Is.EqualTo 30)
