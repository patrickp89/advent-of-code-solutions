namespace AoCSolutions.E2019

module Day02 =

  open FSharpx.Text.Strings
  open System.IO

  exception UnknownIntopcode of string

  let rec compute_opcodes opcodes pos =
    let opcode = opcodes |> List.item pos
    let f =
      match opcode with
      | 1  -> Some (fun a b -> a + b)
      | 2  -> Some (fun a b -> a * b)
      | 99 -> None
      | _  -> raise (UnknownIntopcode "Unknown opcode!") in
        match f with
        | Some g ->
          begin
            let sourcepos1 = opcodes |> List.item (pos + 1)
            let sourcepos2 = opcodes |> List.item (pos + 2)
            let targetpos = opcodes |> List.item (pos + 3)
            let val1 = opcodes |> List.item sourcepos1
            let val2 = opcodes |> List.item sourcepos2
            let x = g val1 val2
            let new_opcodes =
              opcodes
              |> List.mapi (fun i c -> if i = targetpos then x else c)
            compute_opcodes new_opcodes (pos + 4)
          end
        | None -> opcodes
  
  let compute_intcode_program (opcode_list: string) init_state_pos1 init_state_pos2 =
    let intcodes =
      opcode_list
      |> split ','
      |> List.ofArray
      |> List.map (fun line -> (int line))
      |> List.mapi (fun i c ->
        match i with
        | 1 -> init_state_pos1
        | 2 -> init_state_pos2
        | _ -> c) in
    let res = compute_opcodes intcodes 0
    res.Head

  let solvePuzzle01 f =
    let lone = File.ReadLines f |> Seq.head
    compute_intcode_program lone 12 2

  let rec range a b =
    if a > b then []
    else a :: range (a+1) b

  let try_opcodes_computation opcode_list noun sval =
    range 0 99
    |> List.map (fun verb -> ((compute_intcode_program opcode_list noun verb), noun, verb))
    |> List.map (fun (x, n, v) -> (if x = sval then Some (n, v) else None))
    |> List.choose (fun r ->
      match r with
      | Some (n, v) -> Some (n, v)
      | None -> None)

  let solvePuzzle02 f =
    let lone = File.ReadLines f |> Seq.head
    range 0 99
    |> List.map (fun noun -> (try_opcodes_computation lone noun 19690720))
    |> List.concat
    |> List.map (fun (n, v) -> n.ToString() + "," + v.ToString())
    |> List.head
