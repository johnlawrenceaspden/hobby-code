main = f 


f = putStr "Hello"
increment x = x + 1
and1 a b = if a == b then a
                     else False

and2 True True = True
and2 _    _    = False

roots :: (Float, Float, Float) -> (Float, Float)

roots (a,b,c) = if d < 0 then error "sorry" else (x1,x2) where
    x1 = e + sqrt d / (2 * a)
    x2 = e - sqrt d / (2 * a)
    d = b*b - 4*a*c
    e = -b / (2 * a)

p1, p2 :: (Float, Float, Float)
p1 = (1.0, 2.0, 1.0)
p2 = (1.0, 1.0, 1.0)

fact 0 = 1
fact n = n * fact (n-1)

sumList [] = 0
sumList (x:xs) = x + sumList xs

nthListEl (l:ls) 1 = l
nthListEl (l:ls) n = nthListEl ls (n-1)






