import Graphics.UI.Gtk
import System.IO
import Data.Char
import Data.List 
import System.Process 
import Control.Monad.State

type Fila = [[Char]]
type Matriz = [Fila] 

addcolumna::String->Int->Fila->Fila
addcolumna x pos xs = (take pos xs)++(x:(tail (drop pos xs)))

addfila::Fila->Int->Matriz->Matriz
addfila xs pos xxs = (take pos xxs)++(xs:(tail (drop pos xxs)))

molde::Fila
molde = ["0","1","2","3","4","5","6","7"]

base::Matriz
base = [["00","01","02","03","04","05","06","07"],["10","11","12","13","14","15","16","17"],["20","21","22","23","24","25","26","27"],["30","31","32","33","34","35","36","37"],["40","41","42","43","44","45","46","47"],["50","51","52","53","54","55","56","57"],["60","61","62","63","64","65","66","67"],["70","71","72","73","74","75","76","77"]]

--posicion fila 0..7 columna 0..7
--elemento-fila-columna como una matriz-Matriz
--fun q coloca cualquier elemento String en cualquier posicion en la matriz 8x8
addposicion::String->Int->Int->Matriz->Matriz
addposicion e row col maestro = addfila (addcolumna e col (maestro!!row)) row maestro

sacarelem::Int->Int->Matriz->String
sacarelem row col maestro = (maestro!!row)!!col

compararIzq::String->Int->Int->Matriz->Bool
compararIzq e row 0 maestro = False
compararIzq e row col maestro 
	| e/=(sacarelem row (col-1) maestro)= False || compararIzq e row (col-1) maestro
	| otherwise = True

compararRig::String->Int->Int->Matriz->Bool
compararRig e row 7 maestro = False
compararRig e row col maestro
        | e/=(sacarelem row (col+1) maestro)= False || compararRig e row (col+1) maestro
        | otherwise = True

compararUp::String->Int->Int->Matriz->Bool
compararUp e 0 col maestro = False
compararUp e row col maestro
        | e/=(sacarelem (row-1) col maestro)= False || compararUp e (row-1) col maestro
        | otherwise = True 

compararDown::String->Int->Int->Matriz->Bool
compararDown e 7 col maestro = False
compararDown e row col maestro
        | e/=(sacarelem (row+1) col maestro)= False || compararDown e (row+1) col maestro
        | otherwise = True

compararDIzqsup::String->Int->Int->Matriz->Bool
compararDIzqsup e 0 c maestro = False
compararDIzqsup e f 0 maestro = False
compararDIzqsup e row col maestro
        | e/=(sacarelem (row-1) (col-1) maestro)= False || compararDIzqsup e (row-1) (col-1) maestro
        | otherwise = True

compararDRiginf::String->Int->Int->Matriz->Bool
compararDRiginf e f 7 maestro = False
compararDRiginf e 7 c maestro = False
compararDRiginf e row col maestro
        | e/=(sacarelem (row+1) (col+1) maestro)= False || compararDRiginf e (row+1) (col+1) maestro
        | otherwise = True

compararDIzqinf::String->Int->Int->Matriz->Bool
compararDIzqinf e 7 c maestro = False
compararDIzqinf e f 0 maestro = False
compararDIzqinf e row col maestro
        | e/=(sacarelem (row+1) (col-1) maestro)= False || compararDIzqinf e (row+1) (col-1) maestro
        | otherwise = True

compararDRigsup::String->Int->Int->Matriz->Bool
compararDRigsup e 0 c maestro = False
compararDRigsup e f 7 maestro = False
compararDRigsup e row col maestro
        | e/=(sacarelem (row-1) (col+1) maestro)= False || compararDRigsup e (row-1) (col+1) maestro
        | otherwise = True

queenmove::Int->Int->Matriz->Bool
queenmove row col base = (compararDIzqsup e row col base) || (compararDRiginf e row col base) || (compararDIzqinf e row col base) || (compararDRigsup e row col base) || (compararIzq e row col base) || (compararRig e row col base) || (compararUp e row col base) || (compararDown e row col base)
	where e = sacarelem row col base

check :: Num t => t ->Int->Int->Matriz->Matriz->(t,Matriz)
check contador row col anterior siguiente
	| (queenmove row col siguiente)==False = (contador+1,siguiente)
	| (queenmove row col siguiente)==True = (contador,anterior)

menu = "\x1B[32m\t\t\tBienvenido a las 8 Reynas.\nEl movimiento de la Reyna es igual al del ajedrez \n\nDebes colocar a la reyna en filas(0..7) y columnas(0..7) .\nPresiona Ctrl+D para salir.\nHave fun\x1B[0m"

reyna::IO [Char]
reyna = go (0,base)
  where go contenido = do 
	system "clear"	
	putStrLn menu
	let conta = fst contenido
	let anterior = snd contenido
	putStrLn $ show $ anterior!!0
        putStrLn $ show $ anterior!!1
        putStrLn $ show $ anterior!!2
        putStrLn $ show $ anterior!!3
        putStrLn $ show $ anterior!!4
        putStrLn $ show $ anterior!!5
        putStrLn $ show $ anterior!!6
        putStrLn $ show $ anterior!!7
	putStrLn "\n\x1B[36m Posicion de la Reyna\x1B[0m"
	putStr "fila: "
	row <- getLine
	putStr "columna: "
	col <- getLine
	let siguiente = (addposicion "Qn" (read row::Int) (read col::Int) anterior)
	let resultado = (check conta (read row::Int) (read col::Int) anterior siguiente)
	if (fst resultado)==8
        then return "ganaste"
        else go resultado

main = do
	juego <- reyna
	putStrLn juego
	putStrLn "\x1B[35mJuegar de nuevo? y/n:\x1B[0m"
	opcion <- getLine
	if opcion == "y" then main else return ()








