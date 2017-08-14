import System.IO
data Super = Super Int String Float deriving (Show)		
--type Tiposupermercado = [(Codigo,Nombre,Precio)]
codigo :: Super -> Int
codigo (Super codigo _ _) = codigo

nombre :: Super -> String
nombre (Super _ nombre _) = nombre

precio :: Super -> Float
precio (Super _ _ precio) = precio

tupladoble ::  Super -> (String, Float)
tupladoble  (Super x y z) = (y,z)
--	| a==x = (y,z)
--	| otherwise = error "no hay productos"
supermercado::[Super]
supermercado = [(Super 4719 "Barritas de merluza" 11), (Super 5643 "Panales, Talla 2" 20), (Super 3814 "Mermelada naranja" 56.50), (Super 1111 "Balon playa (mediano)" 21), (Super 1112 "Balon playa (gigante)" 133), (Super 1234 "Aceite oliva, 1 litro" 23.50)]

compras1::[Super]
compras1 = [(Super 6666 "profilactico, tio vlady" 2.5), (Super 4720 "atun grated, A5" 6.5), (Super 9450 "docena gaseosas, energina" 11.5), (Super 9540 "1 botella de vodka 750ml, RussKaya" 12)]

listaCodigo::[Super]->[Int]
listaCodigo supermercado = map codigo supermercado

listaFactura::[Super]->[(String,Float)]
listaFactura supermercado =zip (map nombre supermercado) (map precio supermercado)

hacerFactura1::Int->[Super]->(String,Float)
hacerFactura1 x [] = ("no existe el producto "++(show x), 0.0)
hacerFactura1 x (y:supermercado)
	| x==(codigo y) = tupladoble y
--	| x/=(codigo y) = hacerFactura1 x supermercado
	| otherwise = hacerFactura1 x supermercado  

--enlista las tuplas de productos y precios segun una lista de codigos
hacerFactura2::[Int]->[Super]->[(String,Float)]
hacerFactura2 (x:[]) (y:supermercado) =[hacerFactura1 x (y:supermercado)]
hacerFactura2 (x:xs) (y:supermercado) =[hacerFactura1 x (y:supermercado)] ++ hacerFactura2 xs (y:supermercado)

sumarFactura1::Int->[Super]->Float
sumarFactura1 x [] = 0.0
sumarFactura1 x (y:supermercado)
	| x==(codigo y) = precio y
	| otherwise = sumarFactura1 x supermercado

--te devuelve el total de productos comprados
sumarFactura2::[Int]->[Super]->Float
sumarFactura2 (x:[]) (y:supermercado) = sumarFactura1 x (y:supermercado)
sumarFactura2 (x:xs) (y:supermercado) = sumarFactura1 x (y:supermercado) + sumarFactura2 xs (y:supermercado)

tellsuper::(String,Float)-> String
tellsuper (a,b)= a++"......."++(show b)

--te devuelve las lista de productos comprados
formatearFactura::[(String,Float)]-> String
formatearFactura (x:[])=tellsuper x
formatearFactura (x:xs)=tellsuper x ++ "\n"++ formatearFactura xs

--el \n solo sirve en IO, asi q ni modo a usar el do
main = do
	let menu = formatearFactura $ hacerFactura2 (listaCodigo supermercado) supermercado
	let cuenta = show $ sumarFactura2 (listaCodigo supermercado) supermercado
	putStrLn "--Cachina Shoping centre--"
	putStrLn menu
	putStr "total ......."
	putStrLn cuenta
--factura :: (Super Int String Float) => Int -> (String,Float)
--factura::t->(String, Float)
--factura x=(y,z)
--	where x = codigo (Super x y z)
--	      y = nombre (Super x y z)
--	      z = precio (Super x y z)

--listaFactura listaCodigo supermercado = map supermercado  
