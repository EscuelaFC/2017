{-
Abrego Alvarez Jonathan
308043305
-}

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

miCuenta :: Int
miCuenta = 308043305

{-
La suma de dos Nat, definidos recursivamente
-}
suma:: Nat -> Nat -> Nat
suma Cero n = n
suma (S m) n = S (suma n m)

{-
La operacion producto de dos Nat
-}
prod::Nat->Nat->Nat
prod Cero _ = Cero
prod x (S Cero) = x
prod x (S y) = suma x (prod x y)

{-
Verificar que el primer Nat es mayor que el segundo
en su defecto devuelve false
-}
mayorQue::Nat->Nat->Bool
mayorQue Cero Cero = False
mayorQue Cero (S _) = False
mayorQue (S _) Cero = True
mayorQue (S m) (S n) = if igual m n == True then  False
                       else mayorQue m n

{-
Verifica si dos Nat son iguales
-}
igual::Nat->Nat->Bool
igual Cero Cero = True
igual (S _) Cero = False 
igual Cero (S _) = False
igual (S m) (S n) = igual m n

{-
Devuelve la potencia de un numero x a la potencia k
donde la primer entradas es x y la segunda k
-}
power::Int->Int->Int
power x y
        | y == 0 = 1
        | otherwise = x * power x (y -1)

{-
Devuelve la potencia de un numero x a la potencia k
donde la primer entradas es x y la segunda k,
pero con la particularidad que si n es par realiza una
calcula la potencia de forma distinta a que si fuer
impar
-}
power2::Int->Int->Int
power2 n k
         | (mod k 2) == 0 = (power (power n 2) (div k 2))
         | otherwise = n * (power n (k-1))

{-
Dada una lista devuelve la reversa de esta
-}
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs)++[x]

{-
Regresa la suma total de todos los miembros de
nuestra lista
-}
sumal::[Int]->Int
sumal [] = error "Caso no valido"
sumal [x]=x
sumal (x:xs) = x + (sumal(xs))

{-
Toma los primeros n elementos de una lista, n ≥ 0
-}
toma::Int->[a]->[a]
toma _ [] = []
toma n _
        | n <= 0 = []
toma n (x:xs) = x : toma (n-1) xs

{-
Tira los primeros n elementos de una lista, n ≥ 0
-}
tira::Int->[a]->[a]
tira _ [] = []
tira n xs
         | n <= 0 = xs
tira n (_:xs) = tira (n-1) xs

{-
Toma un elemento x y una lista l, nos dice cuántas veces aparece x en l
-}
cuantas::Eq a=>a->[a]->Int
cuantas _ []= 0
cuantas n (x:xs)
               | n == x = 1 + (cuantas n xs)
               | otherwise = (cuantas n xs)

{-
Nos indica las veces que aparece un numero en nuestra lista, 
devolviendonos una tupla de listas donde la primer entrada de 
la tupla sera el numero y la segunda el numero que aparece en 
la lista 
-}
frec::Eq a=>[a]->[(a, Int)]
frec [] = []
frec ls = nub [(x,cuantas x ls)| x<-ls]

{-
Devuelve los elemento que aparecen solo una vez en nuestra lista
-}
unaVez::Eq a=>[a]->[a]
unaVez [] = []
unaVez ls = [x |x<-ls , cuantas x ls == 1]

{-
La entrada es una cadena que contiene palabras separadas por espacios, 
tomamos la primer letra de cada palabra y juntamos en una sola cadena.
-}
compress1::String->String
compress1 xs = aux1 0 xs

{-
La entrada es una cadena que contiene palabras separadas por espacios, 
cada palabra tiene un número al inicio
De cada palabra obtenemos el caracter que esté en la posición que el número al
inicio indica y devolvemos una sola cadena. 
Si el número excede la longitud de la palabra debes devolver un espacio en blanco.
-}
compress2::String->String
compress2 str = aux2 (words str)

{-
Recibimos una tupla con 2 enteros los cuales serviran para determinar nuestro
intervalo
-}
juego::(Int,Int)->(Int,Int)
juego (n,m) = (length (listaFinal(eliminaDuplicados(generaTuplas n m))),mod (multiplica 1 (listaFinal (eliminaDuplicados ( generaTuplas n m)))) ((power 10 9)+ 7))

{-
Funcion empleada para que devuelva la primer letra de una 
palabra, dada una cadena, donde nuestro primer argumento 
es un entero el cual nos servira para poder "iterar" la 
cadena se inicializa desde 0 cuando se emplea debido que 
si no es asi omitara la primer palabra
-}
aux1::Int -> String ->String
aux1 _ " " = ""
aux1 n str 
            | (n >= (length str)) = ""
            |  n == 0 =[str !! 0] ++ aux1 (n+1) str
            | ([str !! n] == " ") =  [str !! (n+1)] ++ aux1 (n+1) str
            | otherwise = aux1 (n+1) str

{-
Funcion empleada para poder extaer el numero que aparece 
al principio de cada palabra
-}
dameNum:: String ->String
dameNum (x:xs) = if (isDigit x) then
                      [x]++dameNum xs
                  else []
{-
Vemos el tamanio de nuestro digito extraido para 
ver el caso de que si el tamanio de nuestro digito es mayor 
que el resto de la cadena simplemente ya no tiene casi buscar
el elemento en dicha posicion porque no habra forma de obtenerlo
y asi regresar solo el espacio en blanco
-}
tamanioDigitos:: String -> Int
tamanioDigitos ""= 0
tamanioDigitos (x:xs)
             | isDigit x = 1 + tamanioDigitos xs
             | otherwise = 0

{-
Usamos esta funcion para devolver el caracter en x posicion en base 
al numero inicial de cada palabra
-}
devuelveCarcater::String ->String
devuelveCarcater ""  = []
devuelveCarcater str
                    | ((read (dameNum str)::Int)+1) > (length str - tamanioDigitos str) = " "
                    | otherwise = [str !! ((read (dameNum str)::Int)+1)]

{-
funcion auxiliar que hace toda la chamba y devuelve la lista 
solicitada en compress2
-}
aux2::[String] ->String
aux2 [] = ""
aux2 (x:xs) = devuelveCarcater x ++ aux2 xs

{-
Funcion empleada para determinar el numero de divisores que 
tiene un numero y así poder determinar mas adelante si es 
un numero primo o no
-}
cantidadDivisores:: Int->Int->Int->Int
cantidadDivisores n k d| k < 1 = error"Debe ser mayor que 1"
                       | (((mod k n)==0) && (n <= k)) = cantidadDivisores (n+1) k (d+1)
                       | (n<k) = cantidadDivisores (n+1) k d
                       | otherwise = d

{-
Dado un entero x determinamos si es primo, usando como ayuda
la funcion cantidadDivisores ya que es primo si solo tiene 
2 divisores que son el 1 y el numero x
-}
esPrimo ::Int -> Bool
esPrimo n = (cantidadDivisores 1 n 0) == 2

{-
Devolvemos la lista de primos donde m lo tomaremos como 
el segundo argumento de la tupla que nos pasan en juega
para así sacar solo los numeros primos que estan [1, m]
-}
devuelveListaPrimos:: Int ->[Int]
devuelveListaPrimos m = [x | x <- [1..m], esPrimo x ]

{-
Generamos la tuplas que cumplas las condiciones dadas donde nuestros argumentos 
donde esta funcion tomara el intervalo pasado en la funcion juego cuando sea empelado
donde el primer y segundo argumento sera el primer y segundo argumento de la tupla 
respectivamente
-}
generaTuplas::Int -> Int -> [(Int,Int)]
generaTuplas m n = [(x,y) | x<-lst, y<-lst, x /= y ,  (x+y)<= n &&  (x+y) >= m]
                        where lst = (devuelveListaPrimos n)

{-
Vemos que si una tupla esta en la lista generada por nuestra
funcion generaTuplas
-}
esta:: (Int,Int)->[(Int,Int)] ->Bool
esta n [] = False
esta n (x:xs) = if (fst n == snd x) &&  (fst x == snd n) then
                    True
                else (esta n xs)

{-
Eliminamos los duplicados de nuetra lista de tuplas donde
consideraremos que una tupla es igual a otro si 
(x,y) == (x1,y2) donde x=y1 & y=x1
-}
eliminaDuplicados :: [(Int,Int)] -> [(Int,Int)]
eliminaDuplicados [] = []
eliminaDuplicados (x:xs)= if (esta x xs) then 
                              eliminaDuplicados xs
                          else [x]++eliminaDuplicados xs                

{-
Consiste en ver si existen elementos con suma igual 
donde ademas consideraremos cual es menor para asi 
elegir este como se pide
-}
sumaIgual:: (Int,Int)->[(Int,Int)] -> Bool
sumaIgual n [] = False
sumaIgual n (x:xs) = if (fst n + snd n) == (fst x + snd x) then
                        if (fst n * snd n) < (fst x * snd x) then 
                           False
                        else True
                      else (sumaIgual n xs)                

{-
Generamos una lista con los elementos que cumplieron todas 
las condiciones mencionadas en la definicion del problema
-}
listaFinal ::[(Int,Int)] -> [(Int,Int)]
listaFinal [] = []
listaFinal (x:xs) | (sumaIgual x xs) = listaFinal xs
                  |  otherwise = [x]++listaFinal xs

{-
Devolvemos la multiplicacion de los elemento en la 
lista de tuplas  
-}
multiplica::Int->[(Int,Int)]-> Int
multiplica n [] = n
multiplica n (x:xs) = n *(fst x*snd x)* (multiplica n xs)

