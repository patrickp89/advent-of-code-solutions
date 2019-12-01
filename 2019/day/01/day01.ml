open Core

let calculate_mass l =
  (Float.iround_down_exn (float_of_int ( l/ 3))) - 2

let solve_puzzle_01 f =
  let in_channel = In_channel.create f in
  In_channel.input_lines in_channel
  |> List.rev_map ~f:(fun line -> (int_of_string line))
  |> List.rev_map ~f:(fun i -> (calculate_mass i))
  |> List.fold ~init:0 ~f:(fun acc mass -> (acc + mass))

let rec calculate_mass_including_fuel l =
  let base_mass = calculate_mass l in
  if base_mass < 0 then
    0
  else
    base_mass + (calculate_mass_including_fuel base_mass)
 
let solve_puzzle_02 f =
  let in_channel = In_channel.create f in
  In_channel.input_lines in_channel
  |> List.rev_map ~f:(fun line -> (int_of_string line))
  |> List.rev_map ~f:(fun i -> (calculate_mass_including_fuel i))
  |> List.fold ~init:0 ~f:(fun acc mass -> (acc + mass))

let%test "simple mass calc" =
  (calculate_mass 94735) = 31576

let%test "rec mass calc" =
  (calculate_mass_including_fuel 1969) = 966

let day01_input_file =
  let d = Sys.getcwd () in
  Filename.concat d "2019-01.input"

let%test "solve day 01 puzzle 01" =
  let mass = solve_puzzle_01 day01_input_file in
  print_endline ("Solution to puzzle 01 from day 01: " ^ (string_of_int mass)) ;
  mass = 3394106

let%test "solve day 01 puzzle 02" =
  let mass = solve_puzzle_02 day01_input_file in
  print_endline ("Solution to puzzle 02 from day 01: " ^ (string_of_int mass)) ;
  mass = 5088280
