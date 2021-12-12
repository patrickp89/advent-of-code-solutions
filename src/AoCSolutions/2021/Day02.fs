namespace AoCSolutions.E2021

module Day02 =

  open FSharpx.Text.Strings
  open System.IO

  type Direction = Up of int | Down of int | Forward of int
  exception UnknownSubmarineDirectionEx

  let toDirectionAndDistance (l: string) =
    let splits = l |> split ' ' |> List.ofArray
    let direction = (splits |> List.item 0).ToUpper ()
    let distance = int (splits |> List.item 1)
    match direction with
    | "UP" -> Up distance
    | "DOWN" -> Down distance
    | "FORWARD" -> Forward distance
    | _ -> raise UnknownSubmarineDirectionEx

  let isHorizontalMovement dir =
    match dir with
    | Up _ -> false
    | Down _ -> false
    | Forward _ -> true

  let isVerticalMovement dir =
    not (isHorizontalMovement dir)

  let getDistance dir =
    match dir with
    | Up y -> -y
    | Down y -> y
    | Forward x -> x

  let computePosition course =
    let parsedDirections =
      course
      |> Seq.map toDirectionAndDistance
    let horizontalPos =
      parsedDirections
      |> Seq.filter isHorizontalMovement
      |> Seq.map getDistance
      |> Seq.sum
    let depth =
      parsedDirections
      |> Seq.filter isVerticalMovement
      |> Seq.map getDistance
      |> Seq.sum
    (horizontalPos, depth)

  let solvePuzzle01 f =
    let (hpos, depth) =
      File.ReadLines f
      |> Seq.cache
      |> computePosition
    hpos * depth
 
  let aimingPositionFolder acc step =
    let (aim, depth, horizontalPos) = acc
    let isVertMov = isVerticalMovement step
    let d = getDistance step
    if isVertMov = true
    then
      let updatedAim = aim + d
      (updatedAim, depth, horizontalPos)
    else
      let updatedDepth = depth + (aim * d)
      let updatedHorizontalPos = horizontalPos + d
      (aim, updatedDepth, updatedHorizontalPos)

  let computePositionUsingAim course =
    let (_ , depth, horizontalPos) =
      course
      |> Seq.map toDirectionAndDistance
      |> Seq.fold aimingPositionFolder (0, 0, 0)
    (horizontalPos, depth)

  let solvePuzzle02 f =
    let (hpos, depth) =
      File.ReadLines f
      |> Seq.cache
      |> computePositionUsingAim
    hpos * depth
