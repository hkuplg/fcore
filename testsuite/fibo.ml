let rec fibo n = if n == 2 || n == 1 then 1 else fibo (n-1) + fibo (n-2) in fibo 10
