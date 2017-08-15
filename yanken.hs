import Graphics.UI.Gtk
import System.Process
import Control.Monad.State
import System.Random
import System.IO

yanken::String->String->String
yanken "piedra" "papel" = "perdiste"
yanken "piedra" "tijera" = "ganaste"
yanken "papel" "piedra" = "ganaste"
yanken "papel" "tijera" = "perdiste"
yanken "tijera" "papel" = "ganaste"
yanken "tijera" "piedra" = "perdiste"

comparar::String->String->String
comparar xs ys 
	| xs==ys = "empate"
	| otherwise = (yanken xs ys)

letras::String->Int
letras "papel" = 1
letras "piedra" = 2
letras "tijera" = 3
letras _ = error "manan"

enteros::Int->String
enteros 1 = "papel"
enteros 2 = "piedra"
enteros 3 = "tijera"
enteros _ = error "manan"

menu = "\x1B[32m\t\t\tBienvenido al yanken-poh.\nLas reglas son las mismas de siempre excepto \n\nChoose your desteny.\nPresiona Ctrl+D para salir.\nHave fun\x1B[0m"

--Yankenpo en la terminal
juego::IO ()
juego = do
	system "clear" 
	putStrLn menu
	a<-randomRIO(1,3)
	putStr "dime un articulo: "
	line<-getLine
	let machine = enteros a
	let jugada = comparar line machine
	putStrLn jugada
	putStr "Quieres seguir jugando y/n? "
	option<-getLine
	if option=="y" then juego else return ()
--	if game=="empate" then juego else return ()


--yankenpo con grafico
game::String->Label->Label->IO ()
game human lblResultado lblMachine = do 
	a<-randomRIO(1,3)
	labelSetText lblMachine	(enteros a)
	labelSetText lblResultado (comparar human (enteros a))

main::IO ()
main = do
	initGUI
	window <- windowNew
	vbox <- vBoxNew False 0 
	set window [windowTitle := "yankenpoh", containerBorderWidth := 5, windowDefaultWidth := 800, windowDefaultHeight := 500, windowWindowPosition := WinPosCenter, containerChild := vbox] 

	lblTitulo <- labelNew $ Just "Yanken-Poh"
	lblInstrucciones <- labelNew $ Just "escoge tu jugada"
	lblMensaje <- labelNew $ Just "eleccion de la maquina: "
	lblMachine <- labelNew $ Just ""
	lblResultado <- labelNew $ Just ""

	btnPiedra <- buttonNew
	btnPiedra `on` sizeRequest $ return (Requisition 100 10)
	btnPapel <- buttonNew
        btnPapel `on` sizeRequest $ return (Requisition 100 10)
	btnTijera <- buttonNew
        btnTijera `on` sizeRequest $ return (Requisition 100 10)
	onClicked btnPiedra $ game "piedra" lblResultado lblMachine
	onClicked btnPapel $ game "papel" lblResultado lblMachine
	onClicked btnTijera $ game "tijera" lblResultado lblMachine

	boxPiedra    <- labelNew $ Just "Piedra"
        containerAdd btnPiedra boxPiedra
        boxPapel    <- labelNew $ Just "Papel"
        containerAdd btnPapel boxPapel
        boxTijera    <- labelNew $ Just "Tijera"
        containerAdd btnTijera boxTijera

        srcfont <- fontDescriptionFromString "Courier Bold 15"
        widgetModifyFg lblMensaje StateNormal (Color 0 102 204)
        widgetModifyFont lblMensaje (Just srcfont)

        srcfont1 <- fontDescriptionFromString "Courier Bold 19"
        widgetModifyFg lblTitulo StateNormal (Color 39 170 218)
        widgetModifyFont lblTitulo (Just srcfont1)
        widgetModifyFg lblResultado StateNormal (Color 39 170 218)
        widgetModifyFont lblResultado (Just srcfont1)

        srcfont2 <- fontDescriptionFromString "Courier italic 14"
        widgetModifyFg lblInstrucciones StateNormal (Color 0 10 10)
        widgetModifyFont lblInstrucciones (Just srcfont2)


        hbox <- hBoxNew True 0
        boxPackStart hbox btnPiedra PackNatural 0
        boxPackStart hbox btnPapel PackNatural 0
        boxPackStart hbox btnTijera PackNatural 0

        hbox2 <- hBoxNew False 0
        boxPackStart hbox2 lblMensaje PackNatural 0
        boxPackStart hbox2 lblMachine PackNatural 0

        containerAdd vbox lblTitulo
        containerAdd vbox lblInstrucciones
        containerAdd vbox hbox
        containerAdd vbox hbox2
        containerAdd vbox lblResultado


        onDestroy window mainQuit
        widgetShowAll window
        mainGUI

