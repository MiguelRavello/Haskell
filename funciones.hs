esSuma::Int->Int->Int
esSuma x y = x + y

esResta::Int->Int->Int
esResta x y = x - y

esMult::Int->Int->Int
esMult x 1 = x
esMult x y = x + esMult x (y - 1)

pote::Int->Int->Int
pote x 1 = x
pote x y = x * pote x (y - 1)

fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x-1)

esDiv::Int->Int->Int
esDiv x y | x==y =1
          | otherwise = 1 + esDiv (x - y) y

dife::Float->Float
dife x = last[x-y**2 | y<-[1..100], y**2<=x]

funcion::Float->Float
funcion c = last[k | k<-[1..100], k**2<=c]

raiz::Float->Float
raiz n 
    | n==0 = 0
    | n>0 = (dife n)/(2*(funcion n)) + funcion n
    | otherwise = error "facto: argumento negativo"
              
circun::Float->Float
circun r = pi * r * r

cilin::Float->Float->Float
cilin r h = pi*2*r*h

cubo::Int->Int
cubo a = a * a * 6

fib::Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1)+ fib (n-2)

fibos::Int->[Int]
fibos 0=[fib 0]
fibos x=[fib x]++fibos(x-1)

pali::String->Bool
pali "" = True
pali (x:[])=True 
pali (x:xs)=x==last(x:xs) && pali (init xs)
