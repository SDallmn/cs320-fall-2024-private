
type tree = 
| Leaf of int
| Node of tree list

let rec height t =
  match t with
  | Leaf _ -> 0
  | Node cs ->
      let rec max_depth cs =
        match cs with
        | [] -> -1
        | c :: cs -> max (height c) (max_depth cs)
      in 1 + max_depth cs


let rec find_ends t = 
  match t with
  | Leaf _ as leaf -> [leaf]
  | Node [] as empt -> [empt]
  | Node kids -> List.concat(List.map find_ends kids)

let rec collapse h t = 
  if h <= 0 then t
  else match t with
  | Leaf _ -> t
  | Node [] -> t
  | Node children ->
    if h=1 then Node (find_ends t)
    else Node (List.map (collapse (h - 1)) children)