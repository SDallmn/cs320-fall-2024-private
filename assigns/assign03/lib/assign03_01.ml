
  let rec add_to_list key value lst =
    match lst with
    | [] -> [(key, value)]
    | (k, v) :: tl when k = key -> (k, v + value) :: tl 
    | hd :: tl -> hd :: add_to_list key value tl 

  let rec mk_unique_keys alst =
    match alst with
    | [] -> [] 
    | (key, value) :: tl -> add_to_list key value (mk_unique_keys tl)