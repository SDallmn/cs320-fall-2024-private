



let is_prime num =
  let rec prime_num num next = 
    next <= 1 ||  ((num mod next) <> 0) && prime_num num (next-1)
  in
  prime_num num (num-1);;