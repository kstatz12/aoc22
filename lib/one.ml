let process list =
  let rec proc list current result = match list with
    | [] -> result
    | ""::t ->
      let nr = (List.fold_left (+) 0 current) :: result in
      proc t [] nr
    | h::t ->
      let nc = (int_of_string h) :: current in
      proc t nc result in
  proc list [] []

let partTwo list =
  let getTop3 list = match list with
    | a::b::c::_ -> [a; b; c]
    | _ -> [] in
  List.fold_left (+) 0 (getTop3 list)

let partOne list =
  match list with
  | [] -> 0
  | h::_ -> h
