

let rec pow n k = if k <> 0 then n * pow n (k - 1) else 1;;