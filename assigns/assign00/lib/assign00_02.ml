

(* 

let is_prime num =
  let rec prime_num num next = 
    next <= 1 ||  ((num mod next) <> 0) && prime_num num (next-1)
  in
  prime_num num (num-1);;  *)

(* let isPrime n =
  (* Returns true if n has no divisors between m and sqrt(n) inclusive. *)
    let rec noDivisors m =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in
  n >= 2 && noDivisors 2*)

(* let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib (n-2);; 

let rec isPrime n = 
  if 
    
    
    m = n / 2 if m = 1 else if m +1 isPrime();; *)

(* let is_prime (n : int) : bool =
  (* Returns true if n has no divisors between m and sqrt(n) inclusive. *)
  let rec noDivisors (m : int) : bool =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in
    n >= 2 && noDivisors 2 *)

let is_prime n =
  let rec go i =
    if i = n then
      true
    else if n mod i = 0 then
      false
    else
      go (i + 1)
  in
  if   n < 2
  then false
  else go 2
    