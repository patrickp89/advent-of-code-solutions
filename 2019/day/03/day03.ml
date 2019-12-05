open Core

exception UnknownDirection of string

let to_direction_function grid_move =
  let direction = String.sub grid_move ~pos:0 ~len:1 in
  let delta = int_of_string (String.sub grid_move ~pos:1 ~len:((String.length grid_move)-1)) in
  match direction with
  | "U" -> (fun ox oy -> (ox, (oy + delta)))
  | "D" -> (fun ox oy -> (ox, (oy - delta)))
  | "R" -> (fun ox oy -> ((ox + delta), oy))
  | "L" -> (fun ox oy -> ((ox - delta), oy))
  | _ ->  raise (UnknownDirection "Unknown direction!")

let calc_point (x, y) dirf = dirf x y

let to_grid_lines (wire_path: string) : (int * int) list =
  let directions = String.split wire_path ~on:',' in
  (* map the list of directions ("U3", "R4", ...) to a list of applicable functions: *)
  List.map directions ~f:(fun direction -> (to_direction_function direction))
  (* then compute the resulting points: *)
  |> List.fold ~init:[(0,0)] ~f:(fun acc dirf -> (calc_point (List.hd_exn acc) dirf)::acc)
  |> List.rev

let rec range a b =
  if a > b then []
  else a :: range (a+1) b

let compute_range a b =
  if (a < b) then
    range a b
  else
    range b a

(* Computes the points the lie BETWEEN two given points
   e.g. [(0,1); (0,2); (0,3)] lie between (0,0) and (0,4). *)
let compute_points_between (x1, y1) (x2, y2) =
  let points = if (x1 = x2) then
    compute_range y1 y2
    |> List.rev_map ~f:(fun i -> (x1, i))
  else
    compute_range x1 x2
    |> List.rev_map ~f:(fun i -> (i, y1)) in
  List.tl_exn points
    |> List.rev
    |> List.tl_exn

let partition_odd_from_even l =
  List.mapi l ~f:(fun i x -> (i,x)) 
  |> List.partition_tf ~f:(fun (i,_) -> (i % 2 = 0))

let compute_all_points_on_lines connected_points =
  (* partition the list of lines {A,B,C,D,E,...} into two separate lists,
  such that A,C,E,... are in the first and B,D,... are in the second: *)
  let (evens, odds) = (partition_odd_from_even connected_points) in
  let even_connected_points_count = ((List.length connected_points) % 2 = 0 ) in
  let m = ((List.length connected_points) / 2) in
  (* compute all points that lie on the lines {a->b, c->d, e->f, ...}: *)
  (* ignore the last point in the list of evens if there is an even number of lines! *)
  let odds_wi = List.map odds ~f:(fun (_,x) -> x) in
  let l = if even_connected_points_count then m - 1 else m in
  let res1 = List.take evens l
  |> List.mapi ~f:(fun i (_,p) -> (compute_points_between p (List.nth_exn odds_wi i)))
  |> List.concat in
  (* then compute all points that lie on the lines {b->c, d->e, ...}: *)
  (* this time ignore the first even (i.e. (0,0))! *)
  let evens_wi = List.tl_exn evens |> List.map ~f:(fun (_,x) -> x) in
  let l2 = if even_connected_points_count
  then m - 1 else m in (* TODO: fix this! in the test below, (2,2) is not part of the resulting list! *)
  let res2 = List.take odds l2
  |> List.mapi ~f:(fun i (_,p) -> (compute_points_between p (List.nth_exn evens_wi i)))
  |> List.concat in
  List.append (List.append connected_points res1) res2

let compute_intersections path1 path2 =
  let points1 = compute_all_points_on_lines path1 in
  let points2 = compute_all_points_on_lines path2 in
  List.filter points1 ~f:(fun p -> (List.exists points2 ~f:(fun e -> e = p)))

let manhatten_distance (x1,y1) (x2,y2) =
  (abs (x1-x2)) + (abs (y1-y2))

let calculate_intersection_closest_to_port intersections =
  let sorted_distances = List.filter intersections ~f:(fun i -> (phys_equal i  (0,0) = false))
  |> List.rev_map ~f:(fun sct -> (sct,(manhatten_distance sct (0,0))) )
  |> List.sort ~compare:(fun (_,d1) (_,d2) -> (Int.compare d1 d2)) in
  List.iter sorted_distances ~f:(fun ((x,y),d) -> (print_endline ("inters.: " ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ " d: " ^ (string_of_int d)))) ;
  List.hd_exn sorted_distances

let solve_puzzle_01 (f: string) : int =
  let lines = In_channel.read_lines f in
  let grid_lines = List.rev_map lines ~f:(fun wire_path -> (to_grid_lines wire_path)) in
  let path1 = List.hd_exn grid_lines in
  let path2 = List.hd_exn (List.tl_exn grid_lines) in
  let (_,d) = compute_intersections path1 path2
  |> calculate_intersection_closest_to_port in
  d

let%test "calc point from startpoints and direction" =
  let f = to_direction_function "U3" in
  let p = f 0 0 in
  p = (0,3)

let%test "map points to lines" =
  (to_grid_lines "U7,R6,D4,L4") = [(0,0); (0,7); (6,7); (6,3); (2,3)]

let%test "map points to lines 02" =
  (to_grid_lines "R8,U5,L5,D3") = [(0,0); (8,0); (8,5); (3,5); (3,2)]

let%test "map points to lines multiple chars" =
  (to_grid_lines "R992,U284,L447,D597,R888,D327") = [(0,0); (992,0); (992,284); (545,284); (545,-313); (1433,-313); (1433,-640)]

let%test "craete range" =
  (range 1 5) = [1; 2; 3; 4; 5]

let%test "compute range for a > b" =
  (compute_range 5 1) = [1; 2; 3; 4; 5]

let%test "compute all points on a single x line BETWEEN the given start end end point" =
  (compute_points_between (0,0) (0,4)) = [(0,1); (0,2); (0,3)]

let%test "compute all points on a single y line BETWEEN the given start end end point" =
  (compute_points_between (33,100) (27,100)) = [(28,100); (29,100); (30,100); (31,100); (32,100)]

let%test "separate odds from evens" =
  let (_, odds) = (partition_odd_from_even [0; 1; 2; 3; 4]) in
  (List.map odds ~f:(fun (_,x) -> x)) = [1; 3]

let%test "separate evens from odds" =
  let (_, evens) = (partition_odd_from_even [1; 2; 3; 4; 5]) in
  (List.map evens ~f:(fun (_,x) -> x)) = [2; 4]

let%test "compute all points on a set of connected lines" =
  (compute_all_points_on_lines [(0,0); (0,7); (6,7); (6,3); (2,3)]) = [(0,0); (0,7); (6,7); (6,3); (2,3);
  (0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (6,4); (6,5); (6,6); (1,7); (2,7); (3,7); (4,7); (5,7); (3,3); (4,3); (5,3)]

let%test "compute all points on a set of connected lines 02" =
  let bla = (compute_all_points_on_lines [(0,0); (0,7); (6,7); (6,3); (2,3); (2,1)]) in
  List.iter bla ~f:(fun (x,y) -> print_endline ("" ^ (string_of_int x) ^ "," ^ (string_of_int y))) ; (* TODO: fix: (2,2) is missing?! *)
  bla = [(0,0); (0,7); (6,7); (6,3); (2,3); (2,1);
  (0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (6,4); (6,5); (6,6); (1,7); (2,7); (3,7); (4,7); (5,7); (3,3); (4,3); (5,3); (2,2)]

let%test "compute intersections" =
  let path1 = [(0,0); (0,7); (6,7); (6,3); (2,3)] in
  let path2 = [(0,0); (8,0); (8,5); (3,5); (3,2)] in
  (compute_intersections path1 path2) = [(0,0); (6,5); (3,3)]

(*let%test "compute intersections 02" =
  let path1 = [(0,0); (0,7); (6,7); (6,3); (2,3); (2,1)] in
  let path2 = [(0,0); (8,0); (8,5); (3,5); (3,2); (3,1)] in
  (compute_intersections path1 path2) = [(0,0); (6,5); (3,3)]*)

let%test "test manhatten distance" =
  let p1 = (1,3) in
  let p2 = (4,5) in
  (manhatten_distance p1 p2) = 5

let%test "find closest intersections" =
  let path1 = [(0,0); (0,7); (6,7); (6,3); (2,3)] in
  let path2 = [(0,0); (8,0); (8,5); (3,5); (3,2)] in
  let (p,_) = compute_intersections path1 path2
  |> calculate_intersection_closest_to_port in
  p = (3,3)
  
let day03_input_file =
  let d = Sys.getcwd () in
  Filename.concat d "2019-03.input"

let%test "solve day 02 puzzle 01" =
  let d = solve_puzzle_01 day03_input_file in
  print_endline ("Solution to puzzle 01 from day 03: " ^ (string_of_int d)) ;
  d = 5357
