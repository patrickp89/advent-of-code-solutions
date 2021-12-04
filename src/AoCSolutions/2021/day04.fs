namespace AoCSolutions.E2021

module day04 =

  open FSharpx.Text.Strings
  open System.IO

  let parseBingoDraws (rawInput: seq<string>) =
    rawInput
    |> Seq.head
    |> split ','
    |> List.ofArray
    |> List.map (fun s -> int s)

  let rec splitBingoBoardLines (boards: string list list) (lines: seq<string>) =
    // our bingo boards have a fixed height:
    let bingoBoardHeight = 5
    // ...and they are separted by one line, each:
    let toatlLineCountPerBoard = bingoBoardHeight + 1

    if (lines |> Seq.length) <= toatlLineCountPerBoard
    then
      let newBoardLines = lines |> List.ofSeq
      newBoardLines :: boards
    else
      let newBoardLines =
        lines
        |> Seq.take toatlLineCountPerBoard
        |> List.ofSeq
      let remainingLines = lines |> Seq.skip toatlLineCountPerBoard
      splitBingoBoardLines (newBoardLines :: boards) remainingLines

  let parseBingoBoardLines (rawInput: seq<string>) =
    let bingoBoardLines: seq<string list> =
      rawInput
      |> Seq.skip 1 // the line representing the draws
      |> splitBingoBoardLines []
      |> Seq.map (fun bl -> (bl |> List.skip 1)) // the separator lines
      |> Seq.rev
    bingoBoardLines
