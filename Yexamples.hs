y f = f(y f)

fact f = \x -> if x == 0 then 1 else x * f (x-1)
yfact = y fact
