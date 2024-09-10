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
  else go 2;;

(* find a new prime for each number of input
we start with the number of primes we want to find
then we find the first prime. then we find another prime
 *)

let nth_prime i = 
  let rec findnextPrime n = 
    let x = n+1 in 
    if is_prime(x) then
      x
    else  
      findnextPrime (n + 1)
in
let rec loop count currPrime =
  if count = i then currPrime
  else
    let nextPrime = findnextPrime currPrime in
    loop (count + 1) nextPrime
in
loop 0 2;;


(* let currPrime = 2
in 
for counter = 1 to i do
  findnextPrime(currPrime)
done
currPrime;; *)

(* 
    then currPrime
   currPrime = findnextPrime (currPrime)
else
  findnextPrime()


let go i x = 
  if i = n then
    x
  else *)