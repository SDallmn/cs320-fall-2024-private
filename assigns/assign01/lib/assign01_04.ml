


open Assign01_03



let to_string s = 
  let rec ints str i = 
    let num = nth s i in 
    let nextnum = nth s (i+1) in
    if num = 0 || nextnum = 0 then
      str ^ (string_of_int num) ^ "]"
    else
      ints (str ^ (string_of_int num) ^ "; ") (i +1)
  in
ints "[" 0;;