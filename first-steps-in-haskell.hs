--Programacion Haskell
--Definición de una función: nombre parámetros = expresión
--La palabra "let" es para probar las funciones en la consola
--Función para mostrar un nombre que se haya ingresado, 20 de Febrero
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

--Función para hacer una multiplicación recibiendo parámetros, 25 de Febrero
multres a = a * 3 

--Función para determinar el discriminante de la raiz cuadrática de la formula general, 25 de Febrero
discrimi a b c = b^2 - 4*a*c

--Función para calcula la suma de los dobles, 25 de Febrero
doubleUs x y = x*2 + y*2

--Función para multiplicar por 2 un numero si es positivo y si es negativo multiplicarlo por 3, 25 de Febrero
postNeg x = if x > 0 then x*2 else x*3

--Función para agregar una valor a una lista dando una posición, 03 de Marzo
insertaelista lista pos valor = take pos lista ++ [valor] ++ drop pos lista

--Función para sustituir el valor de la mitad de una lista con longitud impar, 03 Marzo
listamitadimpar lista pos valor = (take pos lista) ++ [valor] ++ (tail (drop pos lista))

--Dada una lista hacer una funcion que inserte un valor justo a la mitad de la lista, 03 Marzo
insertaemitad lista valor = if ((length lista) `mod` 2) == 0 then insertaelista lista (div (length lista) 2) valor else listamitadimpar lista (div (length lista) 2) valor

--Función BoomBang, 05 Marzo
boombang lista = [if x < 10 then "BOOM!" else "BANG!" | x <- lista, odd x]

--FUNCION PARA TRAER TODOS LOS CARACTERES EN MAYUSCULA DE UNA CADENA, 05 Marzo
solomayus string = [caracter | caracter <- string, elem caracter ['A'..'Z']]

--FUNCION PARA TRAER TODOS LOS CARACTERES EN MINISCULA DE UNA CADENA, 05 Marzo
solominus string = [caracter | caracter <- string, elem caracter ['a'..'z'] || elem caracter [' ']]

--FUNCION PARA TRAER TODOS LOS CARACTERES QUE NO SEAN LETRAS DE UNA CADENA, 05 Marzo
sinletras string = [caracter | caracter <- string, notElem caracter ['A'..'Z'], notElem caracter ['a'..'z']]

--FUNCION PARA OBTENER EL TERCER ELEMENTO DE UNA TUPLA DE 3 ELEMENTOS, Tarea
trd (a, b, c) = c

--Triangulos rectangulos con lados menores que 10 y con perimetro de 24, 10 Marzo
triang = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, (a + b + c) == 24]

--Funcion para calcular la formula general
formulageneral (a, b, c) = if((discrimi a b c) >= 0) then discrimipos (a, b, c) else discrimineg (a, b, c)

discrimipos (a, b, c) = show((((-1*b)+(sqrt(b^2-4*a*c)))/(2*a)),(((-1*b)-(sqrt(b^2-4*a*c)))/(2*a)))

discrimineg (a, b, c) = show((-1*b)/(2*a),(sqrt(-1*(b^2-4*a*c)))/(2*a))++"i"

--Funcion que genere el factorial, 12 Marzo
factorial :: (Integral a) => a -> a
factorial n = if (n < 2) then 1 else n * factorial (n - 1)
--factorial n = producto [1..n]

--Funcion que diga que el 5 es el numero más bonito, 12 Marzo
numCinco 5 = "Este numero es bien bonito"
numCinco x = "Guacala"

--Funcion para dado un numero, 12 Marzo
diaSemana 1 = "Ese dia es lunes"
diaSemana 2 = "Ese dia es martes"
diaSemana 3 = "Ese dia es miercoles"
diaSemana 4 = "Ese dia es jueves"
diaSemana 5 = "Ese dia es viernes"
diaSemana 6 = "Ese dia es sabado"
diaSemana 7 = "Ese dia es domingo"
diaSemana x = "Ese dia no existe"

--Funcion para calcular el factorial con patrones
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

--Funcion para sumar dos vectores con patrones, 12 Marzo
sumaVec (a, b) (c, d) = (a + c, b + d)

--Funcion que replica la funcion Head, 12 Marzo
head' (x:xs) = x
head' [] = error "Error, Lista Vacia"

--Funcion para describir como es una lista y mostrar su contenido, 12 Marzo
func [] = "Lista Vacia"
func [a] = "Lista de un elemento " ++ show a
func [a,b] = "Lista de dos elementos " ++ show a ++ " y " ++ show b
func (a:b:_)  = "La lista es larga. Los primeros dos elementos son: " ++ show a ++ " y " ++ show b

--Funcion para saber la longitud de una lista
length' [] = 0
length' (_:xs) = 1 + length' xs

--Funcion que replica la funcion sum
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Funcion que trae la primera letra de una cadena
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]


--TAREA A2.4 Definición de funciones con guardas, let, case


--Funcion que retorna un texto de acuerdo al índice de masa corporal
bmiTell peso altura
   | peso / altura ^ 2 <= 18.5 = "Peso inferior al normal"
   | peso / altura ^ 2 <= 25.0 = "Peso normal"
   | peso / altura ^ 2 <= 30.0 = "Peso superior al normal"
   | otherwise = "Obesidad"

--Funcion que retorna si el primer valor es igual, menor o igual que el segundo
miCompara a b 
   | a > b = GT
   | a == b = EQ
   | otherwise = LT

--Funcion que retorna las raíces de una ecuación cuadrática ax2 + bx + c
raicesEcuCuad a b c
   | discriminante < 0 = ([(primero + sqrt(-1 * discriminante)) / divisor, (primero - sqrt(-1 * discriminante)) / divisor], "i")
   | otherwise = ([(primero + sqrt(discriminante)) / divisor, (primero - sqrt(discriminante)) / divisor], "r")
   where discriminante = (b ^ 2) - (4 * a * c)
         primero = -1 * b
         divisor = 2 * a

--Funcion que retorna una lista del Índice de masa corporal (IMC) por cada una de las duplas
calBmis listadetuplas = [imc peso altura | (peso, altura) <- listadetuplas]
   where imc peso altura = peso / altura ^ 2

--Funcion que retorna el área de un cilindro donde r es el radio y h la altura
aCilindro r h = 
   let arealateral = 2 * pi * r * h
       areabase = pi * r ^ 2
   in arealateral + 2 * areabase

--Funcion que retorna el área total de un tronco de cono, donde r1 es el radio del círculo menor, r2 es el radio del circulo mayor y g es la generatriz
aTroncoCono r1 r2 g =
   let arealateral = pi * (r1 + r2) * g
       areabasemayor = pi * r2 ^ 2
       areabasemenor = pi * r1 ^ 2
   in arealateral + areabasemayor + areabasemenor

--Funcion que retorna un texto dependiendo de la lista: vacía, unitaria o larga
describeLista lista = "La lista ingresada es " ++ case lista of {[]  -> "una lista vacia.";
                                                                [x] -> "una lista unitaria.";
                                                                lista -> "una lista larga."}

--Funcion que retorna un texto con el nombre del mes y el número del mes. Cualquier valor fuera del rango del 1 al 12 debe mostrar que es un error
mesAnio m = "El mes numero " ++ show m ++ case m of {1 -> " es Enero";
                                                     2 -> " es Febrero";
                                                     3 -> " es Marzo";
                                                     4 -> " es Abril";
                                                     5 -> " es Mayo";
                                                     6 -> " es Junio";
                                                     7 -> " es Julio";
                                                     8 -> " es Agosto";
                                                     9 -> " es Septiembre";
                                                     10 -> " es Octubre";
                                                     11 -> " es Noviembre";
                                                     12 -> " es Diciembre";
                                                     otherwise -> " NO EXISTE (ingresar un valor de 1 a 12)"}


--TAREA A2.5 Definición de funciones recursivas


--1.- Funcion que retorna el máximo valor de una lista.
maximum' [] = error "Es una lista vacia"
maximum' [x] = x
maximum' (head:tail)
   | head > maxTail = head
   | otherwise = maxTail
   where maxTail = maximum' tail

--2.- Funcion que retorna una lista con n veces el valor v.
replicate' n v
   | n <= 0 = []
   | otherwise = [v] ++ replicate' (n - 1) v

--3.- Funcion que retorna los primeros n valores de una lista.
take' n [] = []
take' n (head:tail)
   | n <= 0 = []
   | otherwise = [head] ++ take' (n - 1) tail

--4.- Funcion que retorna la lista inversa de lista.
reverse' [] = []
reverse' (head:tail) = reverse' tail ++ [head]

--5.- Funcion que retorna True si el valor esta en la lista y False si no se encuentra.
elem' v [] = False
elem' v (head:tail)
   | v == head = True
   | otherwise = elem' v tail

--6.- Funcion que retorna una lista con los n primeros números de la serie de Fibonacci
listaFibonacci 1 = [0]
listaFibonacci 2 = [0,1]
listaFibonacci n 
   | n <= 0 = []
   | otherwise = listaFibonacci (n-1) ++ [last (listaFibonacci (n-1)) + last (listaFibonacci (n-2))]

--7.- Funcion que retorna una lista ordenada con el método Quicksort 
quicksort [] = []
quicksort (head:tail) =
   let smallerSorted = quicksort [a | a <- tail, a <= head]
       biggerSorted  = quicksort [a | a <- tail, a > head]
    in smallerSorted ++ [head] ++ biggerSorted

--8.- Funcion que dado el número n en base 10 (decimal), retorna su representación en base 2 (binario)
base10abase2 0 = "0"
base10abase2 1 = "1"
base10abase2 n
   | n `mod` 2 == 0 = base10abase2 (div n 2) ++ "0"
   | otherwise = base10abase2 (div n 2) ++ "1"