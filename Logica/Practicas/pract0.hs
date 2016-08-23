{-
Lógica computacional 2017-1
         Noé Salomón Hernández Sánchez
         Albert M. Orozco Camacho
         C. Moisés Vázquez Reyes
         Diego Murillo
-}
import Data.List
import Data.Char

data Nat = Cero | S Nat deriving Show

suma:: Nat -> Nat -> Nat
suma Cero n = n
suma (S m) n = S (suma n m)

prod::Nat->Nat->Nat
prod Cero _ = Cero
prod x (S Cero) = x
prod x (S y) = suma x (prod x y)

mayorQue::Nat->Nat->Bool
mayorQue Cero Cero = False
mayorQue Cero (S _) = False
mayorQue (S _) Cero = True
mayorQue (S m) (S n) = if igual m n == True then  False
                       else mayorQue m n

igual::Nat->Nat->Bool
igual Cero Cero = True
igual (S _) Cero = False 
igual Cero (S _) = False
igual (S m) (S n) = igual m n

power::Int->Int->Int
power x y
        | y == 0 = 1
        | otherwise = x * power x (y -1)

power2::Int->Int->Int
power2 n k
         | (mod k 2) == 0 = (power (power n 2) (div k 2))
         | otherwise = n * (power n (k-1))

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs)++[x]

sumal::[Int]->Int
sumal [] = error "Caso no valido"
sumal [x]=x
sumal (x:xs) = x + (sumal(xs))

toma::Int->[a]->[a]
toma _ [] = []
toma n _
        | n <= 0 = []
toma n (x:xs) = x : toma (n-1) xs

tira::Int->[a]->[a]
tira _ [] = []
tira n xs
         | n <= 0 = xs
tira n (_:xs) = tira (n-1) xs

cuantas::Eq a=>a->[a]->Int
cuantas _ []= 0
cuantas n (x:xs)
               | n == x = 1 + (cuantas n xs)
               | otherwise = (cuantas n xs)

frec::Eq a=>[a]->[(a, Int)]
frec [] = []
frec ls = nub [(x,cuantas x ls)| x<-ls]

unaVez::Eq a=>[a]->[a]
unaVez [] = []
unaVez ls = [x |x<-ls , cuantas x ls == 1]


compress1::String->String
compress1 xs = aux1 0 xs

compress2::String->String
compress2 str = aux2 (words str)

aux1::Int -> String ->String
aux1 _ " " = ""
aux1 n str 
            | (n >= (length str)) = ""
            |  n == 0 =[str !! 0] ++ aux1 (n+1) str
            | ([str !! n] == " ") =  [str !! (n+1)] ++ aux1 (n+1) str
            | otherwise = aux1 (n+1) str


dameNum:: String ->String
dameNum (x:xs) = if (isDigit x) then
                      [x]++dameNum xs
                  else []

tamanioDigitos:: String -> Int
tamanioDigitos ""= 0
tamanioDigitos (x:xs)
             | isDigit x = 1 + tamanioDigitos xs
             | otherwise = 0

devuelveCarcater::String ->String
devuelveCarcater ""  = []
devuelveCarcater str
                    | ((read (dameNum str)::Int)+1) > (length str - tamanioDigitos str) = " "
                    | otherwise = [str !! ((read (dameNum str)::Int)+1)]

aux2::[String] ->String
aux2 [] = ""
aux2 (x:xs) = devuelveCarcater x ++ aux2 xs

--juego::(Int,Int)->(Int,Int)
--juego = error "Te toca"