namespace AoCSolutions.E2021

module Day04 =

  open FSharpPlus
  open FSharpPlus.Lens
  open System.IO

  type BingoRow = list<int * bool>
  type BingoGrid = list<BingoRow>
  type BingoBoard =
    { Grid: BingoGrid;
      IsWinning: bool;
      WonOnRoundX: int;
      WonWithDrawnNumberY: int; }
  
  module BingoBoard =
    let inline _grid f b =
        f b.Grid <&> (fun g -> { b with Grid = g })

    let inline _isWinning f b =
        f b.IsWinning <&> (fun w -> { b with IsWinning = w })

    let inline _wonOnRoundX f b =
        f b.WonOnRoundX <&> (fun x -> { b with WonOnRoundX = x })

    let inline _wonWithDrawnNumberY f b =
        f b.WonWithDrawnNumberY <&> (fun y -> { b with WonWithDrawnNumberY = y })

  type WinModifer = FirstBoardWins | LastBoardWins

  let parseBingoDraws rawInput =
    rawInput
    |> Seq.head
    |> FSharpx.Text.Strings.split ','
    |> List.ofArray
    |> List.map (fun s -> int s)

  /// Our Bingo boards have a fixed height.
  let bingoBoardHeight = 5

  let rec splitBingoBoardLines boards lines =
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

  let parseBingoBoardLines rawInput =
    let bingoBoardLines: seq<string list> =
      rawInput
      |> Seq.skip 1 // the line representing the draws
      |> splitBingoBoardLines []
      |> Seq.map (fun bl -> (bl |> List.skip 1)) // the separator line(s)
      |> Seq.rev
    bingoBoardLines
  
  let toBoardRow (rawRow: string) =
    // single-digit board numbers are separated by _two_ whitespaces:
    rawRow.Replace("  ", " ")
    |> FSharpx.Text.Strings.split ' '
    |> List.ofArray
    |> List.filter (fun s -> not (s = ""))
    |> List.map (fun sd -> int sd)

  /// Flags a single board cell as unmarked.
  let initializeGridCell row =
    row |> List.map (fun x -> (x, false))

  let toBoardGrid singleBoardLines: BingoGrid =
    singleBoardLines
    |> List.map toBoardRow
    |> List.map initializeGridCell

  let parseBingoBoards rawInput =
    rawInput
    |> parseBingoBoardLines
    |> Seq.map toBoardGrid

  let findAndMarkNumberOnSingleBoard drawnNumber board =
    let updatedGrid =
      board
      |> view BingoBoard._grid
      |> List.map (fun row ->
        row |> List.map (fun (x, b) ->
          if b = true
          then (x, b)
          else (x, (x = drawnNumber))
        )
      )
    board
    |> setl BingoBoard._grid updatedGrid
    |> setl BingoBoard._wonWithDrawnNumberY drawnNumber

  let markNumber drawnNumber boards =
    boards
    |> List.ofSeq
    |> List.map (fun board -> findAndMarkNumberOnSingleBoard drawnNumber board)

  let checkSingleHorizontalBoardLine (row: BingoRow) =
    let distinctMarkedFlags =
      row
      |> List.map (fun (_, b) -> b)
      |> List.distinct
    (distinctMarkedFlags |> List.length) = 1 && (distinctMarkedFlags |> List.head) = true

  let transposeBoard (grid: BingoGrid): BingoGrid =
    [0..(bingoBoardHeight-1)]
      |> List.map (fun i ->
        // extract the i'th column and make it a row:
        grid |> List.map (fun row -> row |> List.item i)
      )

  /// Checks whether a given board meets the Bingo win conditions.
  let checkWinConditions (grid: BingoGrid) =
    let atLeastOneHorizontalLineMarked =
      grid
      |> List.map checkSingleHorizontalBoardLine
      |> List.contains true
    let atLeastOneVerticalLineMarked =
      grid
      |> transposeBoard
      |> List.map checkSingleHorizontalBoardLine
      |> List.contains true
    atLeastOneVerticalLineMarked || atLeastOneHorizontalLineMarked

  /// Checks whether one of the n boards is marked as winner.
  let evaluateWinningBoards boards =
    boards
    |> List.map (fun board ->
      let isWinning =
        board
        |> view BingoBoard._grid
        |> checkWinConditions
      board |> setl BingoBoard._isWinning isWinning
    )

  let isWinning board =
    board |> view BingoBoard._isWinning

  let notWinning board =
    not (board |> isWinning)

  let markDrawnNumberFolder acc drawnNumber =
    let (oldBoards, oldRoundCounter) = acc
    let rc = oldRoundCounter + 1
    let alreadyWonBoards = oldBoards |> List.filter isWinning
    let evaluatedBoards =
      oldBoards
      |> markNumber drawnNumber
      // we only update boards that have not already won:
      |> List.filter notWinning
      |> evaluateWinningBoards
      |> List.map (fun board -> board |> setl BingoBoard._wonOnRoundX rc)
    let allBoards = alreadyWonBoards |> List.append evaluatedBoards
    (allBoards, rc)

  let initializeBoard bingoGrid =
    { Grid=bingoGrid;
      IsWinning=false;
      WonOnRoundX=0;
      WonWithDrawnNumberY=(-1); }

  let computeAllWinningBoards rawInput =
    let draws = rawInput |> parseBingoDraws
    // parse all boards and flag them as non-winning:
    let initialBoards =
      rawInput
      |> parseBingoBoards
      |> Seq.map initializeBoard
      |> List.ofSeq
    // then mark each drawn number and check the Bingo win conditions:
    let (updatedBoards, _) =
      draws |> List.fold markDrawnNumberFolder (initialBoards, 0)
    updatedBoards |> List.filter isWinning

  let findWinningBoard w rawInput =
    let allWinningBoards =
      rawInput
      |> computeAllWinningBoards
      |> List.sortBy (fun board -> board |> view BingoBoard._wonOnRoundX)
    match w with
    | FirstBoardWins -> allWinningBoards |> List.head
    | LastBoardWins -> allWinningBoards |> List.rev |> List.head

  let computeWinningBoardsScore board =
    let bingoGrid = board |> view BingoBoard._grid
    let drawnNumberY = board |> view BingoBoard._wonWithDrawnNumberY
    let sumOfUnmarkedNumbers =
      bingoGrid
      |> List.map (fun row ->
        row
        |> List.filter (fun (_, wf) -> wf = false)
        |> List.map (fun (x, _) -> x)
      )
      |> List.concat
      |> List.sum
    sumOfUnmarkedNumbers * drawnNumberY

  let solvePuzzle01 f =
    File.ReadLines f
      |> Seq.cache
      |> findWinningBoard FirstBoardWins
      |> computeWinningBoardsScore

  let solvePuzzle02 f =
    File.ReadLines f
      |> Seq.cache
      |> findWinningBoard LastBoardWins
      |> computeWinningBoardsScore
