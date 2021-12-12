namespace AoCSolutions.E2021

module Day03 =

  open System.IO

  let countBitAtPos (i: int) (b: string) (report: seq<string>) =
    report
    |> Seq.map (fun s -> (sprintf "%c" s.[i]))
    |> Seq.filter (fun c -> c = b)
    |> Seq.length

  let countBitsAtPos i report =
    let zeroBitCount = report |> countBitAtPos i "0"
    let oneBitCount = report |> countBitAtPos i "1"
    (zeroBitCount, oneBitCount)

  let computeMostCommonBitAtPos (report: seq<string>) i =
    let (zeroBitCount, oneBitCount) = report |> countBitsAtPos i
    if zeroBitCount > oneBitCount
    then 0
    else 1

  let computeLeastCommonBitAtPos (report: seq<string>) i =
    let (zeroBitCount, oneBitCount) = report |> countBitsAtPos i
    if zeroBitCount <= oneBitCount
    then 0
    else 1

  let computeGammaRate (report: seq<string>) =
    // a single line of test input (e.g. "000010010001") has a fixed length:
    let singleReportLineLength = (report |> Seq.head).Length
    let binaryGammaRate =
      [0..(singleReportLineLength-1)]
      |> List.map (computeMostCommonBitAtPos report)
      |> List.map (sprintf "%d")
      |> System.String.Concat
    let decimalGammaRate = System.Convert.ToInt64(binaryGammaRate, 2)
    (binaryGammaRate, decimalGammaRate)

  let computeEpsilonRate (binaryGammaRate: string) =
    let binaryEpsilonRate =
      binaryGammaRate
      |> Seq.map (fun c ->
        match c with
        | '0' -> '1'
        | '1' -> '0'
        | _ -> invalidArg (sprintf "%c" c) "Invalid submarine report bit"
      )
      |> System.String.Concat
    let decimalEpsilonRate = System.Convert.ToInt64(binaryEpsilonRate, 2)
    (binaryEpsilonRate, decimalEpsilonRate)

  let computePowerConsumption (report: seq<string>) =
    let (binaryGammaRate, decimalGammaRate) = report |> computeGammaRate
    let (_, decimalEpsilonRate) = computeEpsilonRate binaryGammaRate
    let g: int = int decimalGammaRate
    let e: int = int decimalEpsilonRate
    g * e

  let solvePuzzle01 f =
    File.ReadLines f
    |> Seq.cache
    |> computePowerConsumption

  let rec determineRating (maxIter: int) (i: int) f (report: seq<string>) =
    let hd = report |> Seq.head
    if i = maxIter
    then
      hd
    else
      let b = f report i |> sprintf "%d"
      let remainingLines =
        report
        |> Seq.filter (fun s -> (sprintf "%c" s.[i]) = b)
      if (remainingLines |> Seq.isEmpty)
      then hd
      else remainingLines |> determineRating maxIter (i+1) f

  let determineOxygenGeneratorRating maxIter i report =
    let f = computeMostCommonBitAtPos
    report |> determineRating maxIter i f

  let determineCo2ScrubberRating maxIter i report =
    let f = computeLeastCommonBitAtPos
    report |> determineRating maxIter i f

  let determineLifeSupportRatings (report: seq<string>) =
    // a single line of test input (e.g. "000010010001") has a fixed length:
    let singleReportLineLength = (report |> Seq.head).Length
    let oxygenGeneratorRating = report |> determineOxygenGeneratorRating singleReportLineLength 0
    let co2ScrubberRating = report |> determineCo2ScrubberRating singleReportLineLength 0
    (oxygenGeneratorRating, co2ScrubberRating)

  let computeLifeSupportRating (report: seq<string>) =
    let (oxygenGeneratorRating, co2ScrubberRating) = report |> determineLifeSupportRatings
    let decimalOgr = System.Convert.ToInt64(oxygenGeneratorRating, 2)
    let decimalCsr = System.Convert.ToInt64(co2ScrubberRating, 2)
    let ogr: int = int decimalOgr
    let csr: int = int decimalCsr
    ogr * csr

  let solvePuzzle02 f =
    File.ReadLines f
    |> Seq.cache
    |> computeLifeSupportRating
