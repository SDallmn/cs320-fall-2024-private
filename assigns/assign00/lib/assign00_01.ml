

let rec sqrt (n:int) :int =
  if n = 0 then 0
  else let r = sqrt (n-1) in
  if (r+1)*(r+1) <= n then r+1 else r ;;