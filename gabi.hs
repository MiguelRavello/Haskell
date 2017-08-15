import System.IO
import Data.Char
import Data.List --(interspace)
import System.Process --(system)
import Control.Monad.State -- (return)

--data Lista = Lista String Int deriving (Show)
type Mujer = (Int,String)
type Lista = [Mujer]
seleccion::Int->Mujer->Bool 
seleccion num gaby
	| num==(fst gaby) = True
	| otherwise = False

search::(Int->Mujer->Bool)->Int->Lista->Bool
search fun num lista = or(map(fun num) lista)

addwo::Int->String->Mujer
addwo num wo = (num,wo) 

enlistar::Int->String->Lista->Lista
enlistar num wo lis = (addwo num wo):lis

searchname::Int->Lista->[String]
searchname num base 
	| [name | (n,name)<-base, n==num]==[] = ["manan"]
	| otherwise =[name | (n,name)<-base, n==num]

searchnum::String->Lista->[Int]
searchnum wo base
	| [num | (num,name)<-base, wo==name]==[] = [000]
	| otherwise = [num | (num,name)<-base, wo==name] 

base::Lista
base = [(950721450,"soyla"),(953442583,"carolinavip"),(959785471,"sra tatiana"),(973526861,"sra karina"),(979511114,"lucero"),(996686535,"vanesa"),(996616946,"andrea"),(946794528,"xiomarita"),(951256549,"trabuco"),(958259090,"kenia"),(944669814,"masajes"),(958358122,"quillabambina"),(941204495,"ximenita"),(951256549,"trabuco"),(958259090,"kenia"),(944669814,"masajes"),(958358122,"quillabambina"),(941204495,"ximenita"),(951256549,"trabuco"),(958259090,"kenia"),(985258938,"juliaquena"),(983109747,"leyla"),(974353820,"sra maria"),(989258908,"giselle"),(944669814,"masajes"),(959640337,"senora malu"),(993496590,"marie"),(966442231,"diablitas"),(958377392,"vedet"),(958429092,"xiomara"),(999446144,"ayacucho"),(976911466,"masajes"),(960449498,"alondra"),(986693878,"kenia"),(957077865,"cielito"),(977133181,"sra rosita"),(930699501,"alejandra"),(959833156,"shows"),(958307316,"sra pati"),(959790770,"senorita"),(962967691,"senora paola"),(941933771,"debora"),(968582313,"rocio"),(958276182,"kajamarcaviop")]

base1::Lista
base1 = [(955722179,"safiro"),(954925565,"brendas"),(935717211,"lobitas"),(935717210,"conejitas"),(946795810,"chibolitas"),(983039231,"fiorella"),(945749170,"coquetas"),(866982,"rosita"),(957773372,"hoteles"),(995334989,"domicilios"),(989891921,"fernandita"),(989875250,"melissa"),(980136061,"juliaquena"),(957970770,"senorita"),(959797852,"senota"),(969140133,"agarradita"),(941138950,"deborah"),(951445859,"universitarias"),(982373027,"venezolana"),(948890897,"kamila"),(958527450,"dailyn"),(958128513,"maricielo"),(974721935,"milagros"),(978888313,"cajamarquina"),(983930053,"yobanita"),(965708110,"bebitas"),(973542728,"valentina"),(959126588,"masajes"),(973176682,"danuska"),(983105858,"korina"),(968580556,"rocio"),(989868544,"jazmin"),(951359291,"charapita"),(998850207,"trabuco"),(983948977,"srafina"),(998011415,"erica"),(959505211,"noelia"),(957483851,"trabuco"),(986746233,"sra camila"),(980669977,"karla"),(991946261,"estefani"),(991948124,"mariana"),(991947163,"bonita"),(991946215,"leydy"),(991947179,"katy2"),(989760876,"nicoll"),(953401134,"karina"),(974338851,"brenda"),(988894985,"ardillitas"),(953745107,"universitarias"),(950759402,"culisueltas"),(959525910,"sarita"),(958730241,"cusquenita 2"),(996482054,"thalia"),(958025630,"sra katy"),(958307316,"roxana"),(983014561,"danielita"),(944061746,"vanesita arg"),(964611540,"mia 22"),(953420582,"sofia 22"),(993734109,"olenka 21"),(957103822,"andrea"),(958521889,"karito"),(959546733,"nataly"),(957773372,"arequipena"),(959898537,"alison"),(949561411,"yobanita"),(944193917,"darling 20"),(989891921,"piuranita2"),(989875250,"lucianita"),(958025630,"alicia milf"),(959736632,"katy cusquenita"),(958307316,"chantall"),(990882788,"paola"),(990882788,"piuranita"),(970803173,"delicia"),(978424544,"daneska"),(973104359,"siomara 18"),(949813090,"ecuatoriana milf"),(953743154,"yuli20"),(976706223,"mishell"),(974397435,"kendra"),(971453471,"melany"),(959747150,"mia"),(957488742,"cajamarquina"),(973176682,"viviana"),(950381263,"pamelita"),(980699796,"xiomara"),(973168719,"vanezita"),(974338851,"jimena"),(973145596,"erika"),(973145596,"tatiana"),(973619292,"brasilera"),(958290118,"amiga vip"),(959701019,"chilenita"),(995460962,"colombiana"),(959736632,"bolivianita")]

internet::Lista
internet = [(123,"copiar"),(994530465,"roselin"),(946799124,"andrea"),(940885488,"sol"),(983143790,"company villa"),(946608484,"jazmin"),(949341848,"dulce"),(974313157,"tamara"),(953426823,"carinona"),(950728370,"nina"),(958178435,"cyberun1"),(973101234,"tia yacki"),(984417930,"dispuestas"),(973240334,"milena"),(984434081,"masiel"),(957019913,"suany col"),(946638139,"ALExandra"),(984434081,"u solv"),(957665025,"silvia anal"),(953409650,"kelly"),(957732256,"romina"),(957711612,"leslie"),(953743784,"abril"),(950761438,"cristal"),(950716886,"caroline"),(983399353,"natalie"),(983853896,"daniela"),(959839110,"naomi"),(997864951,"naomi"),(973171125,"wendy"),(973202916,"thayra"),(958339778,"yanina"),(953013566,"claudia"),(958107168,"shantall"),(974345716,"adamari"),(958356087,"rebeca"),(944034995,"kimberly"),(953415168,"tatiana"),(974773524,"mayte"),(973537670,"sofia"),(946648395,"brissa"),(983399216,"miluska"),(958387795,"andrea"),(953743576,"ivonne"),(959302478,"alessandra"),(987072566,"xiomara"),(983375227,"costenita"),(958166661,"mariana"),(958723449,"ximena"),(990938333,"jazmin"),(991394661,"betancur"),(995319336,"katy"),(981924783,"melinda"),(969728233,"roxy"),(985564679,"francheska"),(952483904,"palomino"),(957038711,"brenda"),(950370185,"cataleya"),(983105858,"alexandra"),(983380098,"brandy"),(950792075,"mistianas"),(975059920,"mistianas"),(983375227,"sofia"),(946695494,"yakelin"),(951495847,"debora"),(975146223,"stephanie"),(983828375,"sacha"),(952422918,"dara"),(952422407,"emily"),(958434969,"charlotte"),(983359399,"pecados"),(951241321,"daniela"),(941221472,"daniela"),(951,"andrea"),(940869235,"madison"),(973515029,"thania"),(983359399,"paris"),(973161400,"piurana"),(950721450,"soyla"),(973176682,"iquitos"),(980699796,"cusquenita")]


menu::[Char]
menu = "\x1B[32m\t\t\tLa lista negra.\nMira, solo add su numero y su nombre \n\nCopea con Ctrl+Shift+C tu base de datos la cual sera imprimida una vez q termines.\nPresiona Ctrl+D para salir.\nHave fun\x1B[0m" --verde

main:: IO Lista
main = go internet
  where go contenido = do
	system "clear"
	putStrLn menu
	putStr "\x1B[33m\nnombre:\x1B[0m " -- \x1B[31m rojo
	name <- getLine 
	putStr "\x1B[33mnumero:\x1B[0m " --amarillo
	num <- getLine
	let resultado = enlistar (read num::Int) name contenido
	putStrLn $ show resultado
	putStr "\x1B[33mdesea add mas: y/n \x1B[0m" -- \x1B[34m azul
	option <- getLine
	if (option=="y") then (go resultado) else return resultado
 
