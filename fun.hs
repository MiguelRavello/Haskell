--una suma de enteros
esSuma::Int->Int->Int
esSuma x y = x+y

--multiplicacion recursiva
esMulti::Int->Int->Int
esMulti x 1 = x
esMulti x y = x+esMulti x (y-1)

--Es una division entera
esDiv::Int->Int->Int
esDiv x y | x==y = 1
          | x>y  = 1+esDiv (x-y) y
          | otherwise = 0
--Division real
esDivreal::Float->Float->Float
esDivreal x y = x/y

--Exponencial
esExponencial::Float->Int->Float
esExponencial x 1=x
esExponencial x 0=1
esExponencial x y | y>0 = x*esExponencial x (y-1)
                  | y<0 = (1/x)*esExponencial x (y+1)

--factorial
esFactorial::Int->Int
esFactorial 0 = 1
esFactorial n | n>0 =  n*esFactorial (n-1)
              | otherwise = error "no piace a la funcion"

palindro::String->Bool
palindro "" = True
palindro (x:[]) = True
palindro (x:xs) = (x==(last (x:xs))) && palindro (init xs)

pal::String->Bool
pal "" = True
pal (x:[])= True
pal xs = (head xs == last xs)&& pal (init (tail xs))


--descabezar
safeInit::String->String
safeInit (x:[])= []
safeInit (x:xs)=xs
