import System.Random
import System.IO
import Data.Char
import Data.List --(interspace)
import System.Process --(system)
import Control.Monad.State -- (return)

--el Juego se inicializa usando la palabra: main la consola
type Estado = (Int,String)
check::Int->Char->String->String->Estado
check contador letra palabra supuesto
	| not $ isAlpha letra || elem letra supuesto = (contador,supuesto)
	| elem letra palabra = (contador, letra:supuesto)
	| otherwise  = (contador + 1, letra:supuesto)
mostrar::String->String->String
mostrar palabra supuesto = map replace palabra
	where replace letra = if elem letra supuesto then letra else '_'

--type GameValue = String
--type GameState = (Int, String)
--playGame :: String -> String -> State GameState GameValue
--playGame [] word = do
--    (_, guessed) <- get
--   return guessed
--get::MonadState s m => m s

--playGame (x:xs) word = do
--    (missed, guessed) <- get
--    put $ check missed x word guessed
--    playGame xs word
--La funcion Jogo remplaza a la monada de estado playGame para actualizar el estado del juego
jogo::String->String->Estado->Estado
jogo [] palabra contenido = contenido
jogo letra palabra contenido = check (fst contenido) (safeHead letra) palabra (snd contenido)

--funciones para el dibujo gallows-helpers
gallows :: (Num a, Ord a) => a -> [Char]
gallows n = "\n\n       |||========|||\n" ++
            (if n > 0 then "       |||         |\n" else "       |||          \n") ++
            (if n > 1 then "       |||         O\n" else "       |||          \n") ++
            (if n < 2 then "       |||           \n" else (helper1 n)) ++
            (if n < 5 then "       |||           \n" else (helper2 n)) ++
            "       |||\n" ++ "       |||\n" ++ "     =================\n\n"
helper1 :: (Num a, Ord a) => a -> [Char]
helper1 n
    | n <= 3    = "       |||        /  \n"
    | n <= 4    = "       |||        /| \n"
    | otherwise = "       |||        /|\\\n"

helper2 :: (Num a, Ord a) => a -> [Char]
helper2 6 = "       |||        / \\\n"
helper2 _ = "       |||        /  \n"

safeHead :: [Char] -> Char
safeHead []     = ' '
safeHead (x:xs) = x

menu = "\x1B[32m\t\t\tBienvenido al Hangperson.\nLas reglas son las mismas de siempre excepto \n\nSi usas multiples letras, solo sera leida la primera letra.\nPresiona Ctrl+D para salir.\nHave fun\x1B[0m"

--go::(Int,String)->IO String , el Int en go serian la inicializacion del numero de errores=0, el crece en funcion de la funcion jogo
usandoLineas::[Char]->IO [Char]
usandoLineas word = go (0, "")
      where go contents = do
            system "clear"
            putStrLn menu
            let misses = fst contents
            let guessed = snd contents
            putStrLn $ gallows misses
            putStrLn $ "Te equivocaste " ++ (show $ misses) ++ " de 7"
            putStrLn $ "Lo q adivinas: " ++ "\x1B[31m" ++ guessed ++ "\x1B[0m"
            putStrLn $ intersperse ' ' $ mostrar word guessed --se muestra [_ _ _] la palabra a adivinar
            putStrLn "Adivina una letra: "
            line <- getLine
            let guess = [safeHead line]
            let result = jogo guess word contents
            if (((fst result) == 7) || '_' `notElem` (mostrar word ((snd result))))
            then (if (fst result) < 7 then return ("Ganaste! la palabra era " ++ word) else return ("Perdiste! la palabra era " ++ word))
            else go result


main = do
	system "reset"
	handle <- openFile "palabras.txt" ReadMode
	archivo <- hGetContents handle
	let xs = words archivo
	a <- randomRIO(0,(length xs) -1)
	let word = xs!!a
	gameOpen <- usandoLineas (map toLower word)
	putStrLn gameOpen
	putStrLn "Juegar de nuevo? y/n:"
	opcion <- getLine
	if opcion == "y" then main else return ()
--	putStr "la palabra tiene "
--	putStr (show (length word))
--	putStrLn " letras"
--	print word
	--hClose handle

--mostrar::Char->String->String
--mostrar u xs =
--	[if elem u xs then x else '-' | u <-xs]
