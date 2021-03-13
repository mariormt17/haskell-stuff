--A2.6 Construcción e implementación de funciones para árboles
--Biblioteca para el manejo de árboles en Haskell

--Declarar el árbol binario
data ArbolB a = VacioB|NodoB(ArbolB a) a (ArbolB a)
          deriving Show

a = NodoB aI 10 aD
  where 
    aI = NodoB aII 15 aID
    aD = NodoB VacioB 18 aDD
    aII = hojaB 24
    aID = hojaB 27
    aDD = hojaB 24

b = NodoB aI 50 aD
  where 
    aI = NodoB aII 30 aID
    aD = NodoB VacioB 60 aDD
    aII = hojaB 30
    aID = hojaB 35
    aDD = hojaB 80

hojaB x = NodoB VacioB x VacioB

--Función que determina la raíz de un árbol binario
raizB VacioB = error "Raiz de arbol vacio"
raizB (NodoB _ x _) = x

--Función que retorna el número de nodos del árbol
tamañoB VacioB = 0
tamañoB (NodoB i _ d) = 1 + tamañoB i + tamañoB d

--Función que retorna el número de niveles del árbol, considerando que la raíz es el nivel 1
profundidadB VacioB = 0
profundidadB (NodoB i _ d) = 1 + max(profundidadB i)(profundidadB d)

--Función que retorna el recorrido en entre orden de un árbol
enOrdenB VacioB = []
enOrdenB (NodoB i r d) = enOrdenB i ++ [r] ++ enOrdenB d

--Función que retorna el recorrido en pre orden de un árbol
preOrdenB VacioB = []
preOrdenB (NodoB i r d) = [r] ++ preOrdenB i ++ preOrdenB d

--Función que retorna el recorrido en post orden de un árbol
postOrdenB VacioB = []
postOrdenB (NodoB i r d) = postOrdenB i ++ postOrdenB d ++ [r]

--Función que empareja valores de una lista o un nodo utiliando Functor y fmap (Funciones de orden superior)
instance Functor ArbolB 
  where
    fmap f VacioB = VacioB
    fmap f (NodoB i r d) = NodoB(fmap f i)(f r)(fmap f d)

empareja :: Functor t =>t a->t(a,a)
empareja = fmap (\x -> (x, x))

--Función que realiza la suma de todos los nodos del árbol utilizando funciones de orden superior
sumaArbolB VacioB = 0
sumaArbolB (NodoB i r d) = sumar(sumaArbolB i) r (sumaArbolB d)
  where
    sumar x y z = x + y + z

--Función que retorna el recorrido en entre orden de un árbol utilizando funciones de orden superior
ennOrdenB VacioB = []
ennOrdenB (NodoB i r d) = concatenar (ennOrdenB i) r (ennOrdenB d)
  where
    concatenar x y z = x ++ [y] ++ z

--Función que realiza la suma de todos los nodos del árbol utilizando la función fold
foldArbolB f z = fun
  where
    fun VacioB = z
    fun (NodoB i r d) = f(fun i) r (fun d)

sumafArbolB = foldArbolB(\x y z -> x + y + z) 0

--Función que retorna el recorrido en entre orden de un árbol utilizando la función fold
ennfOrdenB = foldArbolB(\x y z -> x ++ [y] ++ z) []

--Función que retorna True si cumple con ser un árbol de búsqueda y False si no lo es
todosArbolB p VacioB = True
todosArbolB p (NodoB i r d) = p r && todosArbolB p i && todosArbolB p d

esArbolBB VacioB = True
esArbolBB (NodoB i r d) = todosArbolB(<= r) i && todosArbolB(> r) d && esArbolBB i && esArbolBB d

--Función que retorna True si el valor se encuentra en el árbol binario de búsqueda
perteneceBB :: Ord a => a -> ArbolB a -> Bool
perteneceBB x VacioB = False
perteneceBB x (NodoB i r d) 
   |x == r = True
   |x < r = perteneceBB x i
   |otherwise = perteneceBB x d

--Función que inserta el valor en el árbol binario de búsqueda 
insertarBB x VacioB = NodoB VacioB x VacioB
insertarBB x (NodoB i r d)
   |x <= r = NodoB(insertarBB x i) r d
   |otherwise = NodoB i r (insertarBB x d)

--Función que construye un árbol de búsqueda a partir de una lista de valores 
listaAArbolBB :: Ord a => [a]->ArbolB a
listaAArbolBB = foldr insertarBB VacioB

--Función que obtiene una lista ordenada aplicando el recorrido de arboles
arbolOrdenado :: Ord a => [a] -> [a]
arbolOrdenado = enOrdenB . listaAArbolBB



















































