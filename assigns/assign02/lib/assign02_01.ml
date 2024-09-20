

type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank
type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)
type row_index = 
| Top
| Middle
| Bottom
type col_index = 
| Left
| Middle
| Right
type pos_index = row_index * col_index
(* val get_pos : board -> pos_index -> pos *)

(* let get_pos b pi = 
  let row, col = pi in 

  match pos
 *)
 let get_from_tuple (a, b, c) index =
  match index with
  | Left -> a
  | Middle -> b
  | Right -> c

(* The get_pos function *)
let get_pos (b : board) (pos_idx : pos_index) : pos =
  let row, col = pos_idx in
  let row_tuple = 
    match row with
    | Top -> let (r, _, _) = b in r
    | Middle -> let (_, r, _) = b in r
    | Bottom -> let (_, _, r) = b in r
  in
  get_from_tuple row_tuple col

let three_row p1 p2 p3 = 
  match p1, p2, p3 with
  | Piece X, Piece X, Piece X -> true
  | Piece O, Piece O, Piece O -> true
  | _ -> false

let winner (b) : bool = 
  let((t1,t2,t3), (m1,m2,m3), (b1,b2,b3)) = b in
  let check_rows = three_row t1 t2 t3 || three_row m1 m2 m3 || three_row b1 b2 b3 in
  let check_cols = three_row t1 m1 b1 || three_row t2 b2 m2 || three_row t3 b3 m3 in
  let check_diag = three_row t1 m2 b3 || three_row t3 m3 b1 in 
  check_rows || check_cols || check_diag


(* let winner b = 
  match b with
  | Board -> true *)



(* get the spot at that point in the board and return what it is*)