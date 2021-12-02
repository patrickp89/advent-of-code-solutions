namespace AoCSolutions.E2021

module day01 =

  open System.IO

  let depthFolder acc depth =
    let (priorDepth, depthsCount) = acc
    if depth > priorDepth
    then (depth, depthsCount + 1)
    else (depth, depthsCount)

  let computeIncreasingDepthsCount depths =
    let (_, incDepthsCount) =
      depths
      |> Seq.fold depthFolder (0, 0)
    // ...and ignore the first measurement, as instructed:
    incDepthsCount - 1

  let solvePuzzle01 f =
    File.ReadLines f
    |> Seq.cache
    |> Seq.map (fun line -> (int line))
    |> computeIncreasingDepthsCount
 
  let sumUpWindow (windowIndices: int list) (depths: seq<int>) =
    windowIndices
    |> List.map (fun i -> depths |> Seq.tryItem i |> Option.defaultValue 0)
    |> List.sum

  let computeWindowedIncreasingDepthsCount depths =
    let (_, windowedDepthsCount) =
      depths
      |> Seq.mapi (fun i x -> [i; i+1; i+2])
      |> Seq.map (fun windowIndices -> sumUpWindow windowIndices depths)
      |> Seq.fold depthFolder (0, 0)
    // ...and ignore the first measurement, as instructed:
    windowedDepthsCount - 1

  let solvePuzzle02 f =
    File.ReadLines f
    |> Seq.cache
    |> Seq.map (fun line -> (int line))
    |> computeWindowedIncreasingDepthsCount
