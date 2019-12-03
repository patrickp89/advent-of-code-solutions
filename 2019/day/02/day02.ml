open Core

exception UnknownIntopcode of string

let rec compute_opcodes opcodes pos =
  let opcode = List.nth_exn opcodes pos in
  let f = match opcode with
  | 1  -> Some (fun a b -> a + b)
  | 2  -> Some (fun a b -> a * b)
  | 99 -> None
  | _  -> raise (UnknownIntopcode "Unknown opcode!") in
  match f with
  | Some g -> begin
    let sourcepos1 = List.nth_exn opcodes (pos + 1) in
    let sourcepos2 = List.nth_exn opcodes (pos + 2) in
    let targetpos = List.nth_exn opcodes (pos + 3) in
    let val1 = List.nth_exn opcodes sourcepos1 in
    let val2 = List.nth_exn opcodes sourcepos2 in
    let x = g val1 val2 in
    let new_opcodes = List.mapi opcodes ~f:(fun i c -> if i = targetpos then x else c) in
    compute_opcodes new_opcodes (pos + 4)
  end
  | None -> opcodes
 
let compute_intcode_program opcode_list init_state_pos1 init_state_pos2 =
  let intcodes = String.split opcode_list ~on:','
  |> List.map ~f:(fun line -> (int_of_string line))
  |> List.mapi ~f:(fun i c ->
    match i with
    | 1 -> init_state_pos1
    | 2 -> init_state_pos2
    | _ -> c) in
  let res = compute_opcodes intcodes 0 in
  List.hd_exn res

let solve_puzzle_01 f =
  let lone = In_channel.read_lines f
  |> List.hd_exn in
  compute_intcode_program lone 12 2

let rec range a b =
  if a > b then []
  else a :: range (a+1) b

let try_opcodes_computation opcode_list noun sval =
  range 0 99
  |> List.rev_map ~f:(fun verb -> ((compute_intcode_program opcode_list noun verb), noun, verb))
  |> List.rev_map ~f:(fun (x, n, v) -> (if x = sval then Some (n, v) else None))
  |> List.filter_map ~f:(fun r ->
  match r with
  | Some (n, v) -> Some (n, v)
  | None -> None)

let solve_puzzle_02 f =
  let lone = In_channel.read_lines f
  |> List.hd_exn in
  range 0 99
  |> List.rev_map ~f:(fun noun -> (try_opcodes_computation lone noun 19690720))
  |> List.concat (* = flat_map() *)
  |> List.rev_map ~f:(fun (n, v) -> ("" ^ (string_of_int n) ^ "," ^ (string_of_int v)))
  |> List.hd_exn

let%test "simple intcode prog computation" =
  (compute_opcodes [1; 5; 6; 0; 99; 15; 10; 44] 0) = [25; 5; 6; 0; 99; 15; 10; 44]

let%test "intcode prog computation from string" =
  (compute_intcode_program "1,9,10,3,2,3,11,0,99,30,40,50" 9 10) = 3500

let%test "intcode prog computation from string 02" =
  (compute_intcode_program "1,1,1,4,99,5,6,0,99" 1 1) = 30

 let%test "craete range" =
  (range 1 5) = [1; 2; 3; 4; 5]

let day02_input_file =
  let d = Sys.getcwd () in
  Filename.concat d "2019-02.input"

let%test "solve day 02 puzzle 01" =
  let elf_prog_code = solve_puzzle_01 day02_input_file in
  print_endline ("Solution to puzzle 01 from day 02: " ^ (string_of_int elf_prog_code)) ;
  elf_prog_code = 2782414

let%test "solve day 02 puzzle 02" =
  let noun_and_verb = solve_puzzle_02 day02_input_file in
  print_endline ("Solution to puzzle 02 from day 02: " ^ noun_and_verb) ;
  noun_and_verb = "98,20"
