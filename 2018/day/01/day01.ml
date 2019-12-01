open Core

let solve_puzzle_01 f =
  let in_channel = In_channel.create f in
  In_channel.fold_lines in_channel ~init:0 ~f:(fun acc line -> (acc + (int_of_string line)))

let day01_input_file =
    let d = Sys.getcwd () in
    Filename.concat d "2018-01.input"

let%test "solve day 01 puzzle" =
  let freq = solve_puzzle_01 day01_input_file in
  print_endline ("Solution to the puzzle from day 01: " ^ (string_of_int freq)) ;
  freq = 582
