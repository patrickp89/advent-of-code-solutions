namespace AoCSolutions.E2018

module Day01 =

  open System.IO

  let solvePuzzle01 f =
    File.ReadLines f
    |> Seq.fold (fun acc line -> (acc + (int line))) 0
