let rec fact acc n =
  if n == 0 then 
    acc
  else
    fact (acc*n) (n-1)
in 
fact 1 10
