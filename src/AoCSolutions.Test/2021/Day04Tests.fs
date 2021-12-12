namespace AoCSolutions.E2021.Test

module Day04Tests =

    open AoCSolutions.E2021.Day04
    open NUnit.Framework
    open System.IO

    [<SetUp>]
    let Setup () =
      ()

    let day04InputFile01 =
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/inputs/day/04/2021-04.input"
          )
    
    let testBingoInput = 
          Path.Combine(
              TestContext.CurrentContext.TestDirectory,
              "../../../",
              @"2021/inputs/day/04/2021-04.input.demo"
          )

    [<Test>]
    let TestBingoDrawsParsing () =
      let draws =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> parseBingoDraws
      Assert.That(draws |> List.isEmpty, Is.EqualTo false)
      let draw1 = draws |> List.head
      Assert.That(draw1, Is.EqualTo 7)

    [<Test>]
    let TestBingoBoardLineParsing () =
      let boardLines =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> parseBingoBoardLines
      Assert.That(boardLines |> Seq.isEmpty, Is.EqualTo false)
      Assert.That(boardLines |> Seq.length, Is.EqualTo 3)
      let board1: string list = boardLines |> Seq.head
      Assert.That(board1 |> List.length, Is.EqualTo 5)
      let board1Line1: string = board1 |> List.head
      let expBoard1Line1 = "22 13 17 11  0"
      Assert.That(board1Line1, Is.EqualTo expBoard1Line1)

    [<Test>]
    let TestStringRowToGridRow () =
      let row1 = toBoardRow "22 13 17 11  0"
      Assert.That(row1, Is.EqualTo [22; 13; 17; 11; 0])
      let row2 = toBoardRow " 8  2 23  4 24"
      Assert.That(row2, Is.EqualTo [8; 2; 23; 4; 24])

    [<Test>]
    let TestBingoBoardGridParsing () =
      let boards: BingoGrid list =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> parseBingoBoards
        |> List.ofSeq
      Assert.That(boards |> List.isEmpty, Is.EqualTo false)
      let board1: list<list<int * bool>> = boards |> List.head
      Assert.That(board1 |> List.isEmpty, Is.EqualTo false)

    [<Test>]
    let TestIsBoardWinningCheck () =
      let board1 = [
        [(14, false); (21, false); (17, false); (24, false); (4, false)];
        [(10, false); (16, false); (15, false); (9, false); (19, false)];
        [(18, false); (8, false); (23, false); (26, false); (20, false)];
        [(22, false); (11, false); (13, false); (6, false); (5, false)];
        [(2, false);  (0, false); (12, false); (3, false); (7, false)]
      ]
      let isWinning = checkWinConditions board1
      Assert.That(isWinning, Is.EqualTo false)
      let board2 = [
        [(14, false); (21, false); (17, false); (24, false); (4, false)];
        [(10, false); (16, false); (15, false); (9, false); (19, false)];
        [(18, false); (8, false); (23, false); (26, false); (20, false)];
        [(22, false); (11, false); (13, false); (6, false); (5, false)];
        [(2, true);   (0, true); (12, true); (3, true); (7, true)]
      ]
      let isWinning2 = checkWinConditions board2
      Assert.That(isWinning2, Is.EqualTo true)
      let board3 = [
        [(14, false); (21, false); (17, false); (24, false); (4, true)];
        [(10, false); (16, false); (15, false); (9, false); (19, true)];
        [(18, false); (8, false); (23, false); (26, false); (20, true)];
        [(22, false); (11, false); (13, false); (6, false); (5, true)];
        [(2, false);  (0, false); (12, false); (3, false); (7, true)]
      ]
      let isWinning3 = checkWinConditions board3
      Assert.That(isWinning3, Is.EqualTo true)

    [<Test>]
    let TestBoardTransposition () =
      let board1 = [
        [(14, true); (21, false); (17, false); (24, false); (4, false)];
        [(10, false); (16, false); (15, false); (9, false); (19, false)];
        [(18, false); (8, false); (23, false); (26, false); (20, false)];
        [(22, false); (11, false); (13, false); (6, false); (5, false)];
        [(2, false); (0, false); (12, false); (3, false); (7, false)]
      ]
      let transposedBoard = transposeBoard board1
      let expTransposedBoard = [
        [(14, true); (10, false); (18, false); (22, false); (2, false)];
        [(21, false); (16, false); (8, false); (11, false); (0, false)];
        [(17, false); (15, false); (23, false); (13, false); (12, false)];
        [(24, false); (9, false); (26, false); (6, false); (3, false)];
        [(4, false); (19, false); (20, false); (5, false); (7, false)]
      ]
      Assert.That(transposedBoard, Is.EqualTo expTransposedBoard)

    [<Test>]
    let TestWiningBoardComputation () =
      let board: BingoBoard =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> findWinningBoard WinModifer.FirstBoardWins
      let { Grid=winningBoard; IsWinning=_; WonOnRoundX=_; WonWithDrawnNumberY=lastDrawnNumber; } = board
      let expWinningBoardFirstRow = [(14, true); (21, true); (17, true); (24, true); (4, true)]
      Assert.That(winningBoard |> List.head, Is.EqualTo expWinningBoardFirstRow)
      Assert.That(lastDrawnNumber, Is.EqualTo 24)
    
    [<Test>]
    let TestDay04Puzzle1 () =
      let lines = File.ReadLines day04InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let lsrtng = solvePuzzle01 day04InputFile01
      Assert.That(lsrtng, Is.EqualTo 50008)
    
    [<Test>]
    let TestWiningBoardComputationForLastWinningBoard () =
      let board: BingoBoard =
        (File.ReadLines testBingoInput)
        |> Seq.cache
        |> findWinningBoard WinModifer.LastBoardWins
      let { Grid=winningBoard; IsWinning=_; WonOnRoundX=_; WonWithDrawnNumberY=lastDrawnNumber; } = board
      let expWinningBoardFirstRow = [(3, false); (15, false); (0, true); (2, true); (22, false)]
      Assert.That(winningBoard |> List.head, Is.EqualTo expWinningBoardFirstRow)
      Assert.That(lastDrawnNumber, Is.EqualTo 13)

    [<Test>]
    let TestDay04Puzzle2 () =
      let lines = File.ReadLines day04InputFile01
      Assert.That(lines |> Seq.isEmpty, Is.EqualTo false)
      let lsrtng = solvePuzzle02 day04InputFile01
      Assert.That(lsrtng, Is.EqualTo 17408)
