namespace AoCSolutions.E2019

module day03 =

  open FSharpx.Text.Strings
  open System.IO

  exception UnknownDirection of string

  let to_direction_function (grid_move: string) =
    let direction = grid_move.Substring(0, 1)
    let delta = int (grid_move.Substring(1, ((String.length grid_move)-1)))
    match direction with
    | "U" -> (fun ox oy -> (ox, (oy + delta)))
    | "D" -> (fun ox oy -> (ox, (oy - delta)))
    | "R" -> (fun ox oy -> ((ox + delta), oy))
    | "L" -> (fun ox oy -> ((ox - delta), oy))
    | _ ->  raise (UnknownDirection "Unknown direction!")

  let calc_point (x, y) dirf = dirf x y

  let to_grid_lines (wire_path: string) : (int * int) list =
    let directions = wire_path |> split ','
    (* map the list of directions ("U3", "R4", ...) to a list of applicable functions: *)
    directions
    |> List.ofArray
    |> List.map (fun direction -> (to_direction_function direction))
    (* then compute the resulting points: *)
    |> List.fold (fun acc dirf -> (calc_point acc.Head dirf)::acc) [(0,0)]
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
    let points =
      if (x1 = x2)
      then
        compute_range y1 y2
        |> List.map (fun i -> (x1, i))
        |> List.rev
      else
        compute_range x1 x2
        |> List.map (fun i -> (i, y1))
        |> List.rev
    (points.Tail |> List.rev).Tail

  let partition_odd_from_even l =
    l
    |> List.mapi (fun i x -> (i,x)) 
    |> List.partition (fun (i,_) -> (i % 2 = 0))

  let compute_all_points_on_lines connected_points =
    (* partition the list of lines {A,B,C,D,E,...} into two separate lists,
    such that A,C,E,... are in the first and B,D,... are in the second: *)
    let (evens, odds) = (partition_odd_from_even connected_points)
    let even_connected_points_count = ((List.length connected_points) % 2 = 0 )
    let m = ((List.length connected_points) / 2) in
    (* compute all points that lie on the lines {a->b, c->d, e->f, ...}: *)
    (* ignore the last point in the list of evens if there is an even number of lines! *)
    let odds_wi = odds |> List.map (fun (_,x) -> x)
    let l = if even_connected_points_count then m - 1 else m
    let res1 =
      evens |> List.take l
      |> List.mapi (fun i (_,p) -> (compute_points_between p (List.item i odds_wi)))
      |> List.concat
    (* then compute all points that lie on the lines {b->c, d->e, ...}: *)
    (* this time ignore the first even (i.e. (0,0))! *)
    let evens_wi = (evens |> List.map (fun (_,x) -> x)).Tail
    let l2 =
      if even_connected_points_count
      then m - 1
      else m (* TODO: fix this! in the test below, (2,2) is not part of the resulting list! *)
    let res2 =
      odds |> List.take l2
      |> List.mapi (fun i (_,p) -> (compute_points_between p (List.item i evens_wi)))
      |> List.concat
    List.append (List.append connected_points res1) res2

  let compute_intersections path1 path2 =
    let points1 = compute_all_points_on_lines path1
    let points2 = compute_all_points_on_lines path2
    points1 |> List.filter (fun p -> (points2 |> List.exists (fun e -> e = p)))

  let manhatten_distance (x1,y1) (x2,y2) =
    (abs (x1-x2)) + (abs (y1-y2))

  let calculate_intersection_closest_to_port intersections =
    let sorted_distances =
      intersections
      |> List.filter (fun i -> not (i = (0,0)))
      |> List.map (fun (x,y) -> ((x,y), (manhatten_distance (x,y) (0,0))) )
      |> List.sortWith (fun (_,d1) (_,d2) -> (compare d1 d2))
    // sorted_distances |> List.iter (fun ((x,y),d) -> (print_endline ("inters.: " ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ " d: " ^ (string_of_int d)))) ;
    sorted_distances.Head

  let solvePuzzle01 (f: string) : int =
    let grid_lines =
      File.ReadLines f
      |> Seq.map (fun wire_path -> (to_grid_lines wire_path))
    let path1 = (grid_lines |> List.ofSeq).Head
    let path2 = ((grid_lines|> List.ofSeq).Tail).Head
    let (_,d) =
      compute_intersections path1 path2
      |> calculate_intersection_closest_to_port
    d
