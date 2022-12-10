let calculateScore pair =
  match pair with
  | ("A", "X") -> 4
  | ("A", "Y") -> 8
  | ("A", "Z") -> 3
  | ("B", "X") -> 1
  | ("B", "Y") -> 5
  | ("B", "Z") -> 9
  | ("C", "X") -> 7
  | ("C", "Y") -> 2
  | ("C", "Z") -> 6
  | (_, _) -> 0


let calculateScore2 pair =
  match pair with
  | ("A", "X") -> 3
  | ("A", "Y") -> 4
  | ("A", "Z") -> 8
  | ("B", "X") -> 1
  | ("B", "Y") -> 5
  | ("B", "Z") -> 9
  | ("C", "X") -> 2
  | ("C", "Y") -> 6
  | ("C", "Z") -> 7
  | (_, _) -> 0

let process list calcFn =

  let to_tuples list =
    let rec assign list ret = match list with
      | [] -> ret
      | h::t ->
        let s = String.split_on_char ' ' h in
        let n = (List.nth s 0, List.nth s 1)::ret in
        assign t n in
    assign list [] in

  let rec proc list scores = match list with
    | [] -> List.fold_left (+) 0 scores
    | h::t ->
      proc t ((calcFn h) :: scores) in

  proc (to_tuples list) []
