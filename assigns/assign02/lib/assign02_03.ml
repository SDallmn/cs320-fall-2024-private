

type dir = 
| North
| South
| East
| West


type path = dir list

let distance a b = 
  let c = (a *. a) +. (b *. b) in 
  sqrt c

let dist dirs =
  let rec find_pos dirs (x, y) = 
    match dirs with
    | [] -> (x, y)
    | North :: rest -> find_pos rest (x, y + 1)
    | South :: rest -> find_pos rest (x, y - 1)
    | East :: rest -> find_pos rest (x + 1, y)
    | West :: rest -> find_pos rest (x - 1, y)
  in
  let (final_a, final_b) = find_pos dirs (0,0) in
  distance (float_of_int final_a) (float_of_int final_b)