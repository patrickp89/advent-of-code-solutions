namespace AoCSolutions.E2019

module day01 =

  open System.IO

  let calculateMass l =
    let x = (float (l / 3))
    let y = int (floor x)
    y - 2

  let solvePuzzle01 f =
    File.ReadLines f
    |> Seq.map (fun line -> (int line))
    |> Seq.map (fun i -> (calculateMass i))
    |> Seq.fold (fun acc mass -> (acc + mass)) 0

  let rec calculateMassIncludingFuel l =
    let base_mass = calculateMass l in
    if base_mass < 0 then
      0
    else
      base_mass + (calculateMassIncludingFuel base_mass)
   
  let solvePuzzle02 f =
    File.ReadLines f
    |> Seq.map (fun line -> (int line))
    |> Seq.map (fun i -> (calculateMassIncludingFuel i))
    |> Seq.fold (fun acc mass -> (acc + mass)) 0
