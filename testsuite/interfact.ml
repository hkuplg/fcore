let rec fact n count = 
  if n == 0 then 
    1 
  else 
    if count == 0 then 
      fact n 500
    else
      fact (n-1) (count-1)
in
fact 10 500
