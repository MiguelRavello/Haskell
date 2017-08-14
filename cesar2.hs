myList::String
myList ="abcdefghijklmnopqrstuvwxyz"

search::Char->String->Int
search k (x:xs) | k==x = 0
                |True  = 1+search k xs 

give::Int->String->Char
give 0 (x:xs)=x
give k (x:xs)=give (k-1) xs

caesar::Char->Int->Char
caesar ' ' k= ' '  
caesar x k=give (mod ((search x myList)+k) (length myList))  myList

encriptarCaesar::String->Int->String
encriptarCaesar [] _=[]
encriptarCaesar (x:xs) k=[caesar x k]++encriptarCaesar xs k