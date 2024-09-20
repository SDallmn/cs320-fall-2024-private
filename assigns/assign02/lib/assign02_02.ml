type matrix = {
entries : float list list;
rows : int;
cols : int;
}

let rec chunk_list lst n =
  match lst with
  | [] -> []
  | _ -> 
    let row = List.take n lst in
    let rest = List.drop n lst in
    row :: chunk_list rest n
(* mk_matrix function implementation *)
let mk_matrix entries (r, c) =
  {
    entries = chunk_list entries c;  (* Break the list into rows of length c *)
    rows = r;
    cols = c;
  }