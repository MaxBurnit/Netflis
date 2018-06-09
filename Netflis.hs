import Text.Show.Functions
data Serie = UnaSerie {
    nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}
 
cosasExtranias = UnaSerie {
    nombre = "Stranger things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

espejoNegro = UnaSerie {
    nombre = "Black mirror",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [4,3,3],
    esOriginalDeNetflis = True
}

--Parte 1
maraton1 = [tioGolpetazo,cosasExtranias,dbs,espejoNegro,rompiendoMalo,treceRazonesPorque]
maraton2 = [tioGolpetazo]
listaMaraton = [maraton1,maraton1]
listaMaraton2 = [maraton1,maraton2]

cantSeries maraton = length maraton 

cantCalificaciones serie = length (calificaciones serie)

esPopular serie = cantCalificaciones serie >= 3

valeLaPena serie = (cantTemporadas serie > 1) && (esPopular serie == True)

valeLaPenaMaraton maraton | elem rompiendoMalo maraton = True
                          | otherwise = valeLaPena (head maraton) && valeLaPena  (last maraton)

mitadDeUnaMaraton maraton = splitAt (div (length maraton)  2) maraton

repuntaAlFinal maraton = valeLaPenaMaraton (fst $ mitadDeUnaMaraton maraton) == False && valeLaPenaMaraton (snd $ mitadDeUnaMaraton maraton) == True

promedioSerie serie = div (sum $ calificaciones serie) (length $ calificaciones serie)

dispersionCalificaciones serie = maximum (calificaciones serie) - minimum (calificaciones serie)

calificarSerie serie calificacion = (calificaciones serie) ++ [calificacion]

primeraCalificacion serie = head (calificaciones serie)

aumentarCalificacion funcion xs = (funcion xs + 2) - (5 - (funcion xs + 2))

medioDeLista lista = (init.tail) lista

hypearSerie serie = serie {calificaciones = [aumentarCalificacion head (calificaciones serie)] ++ (medioDeLista $ calificaciones serie) ++ [aumentarCalificacion last (calificaciones serie)]}

--Parte 2
obtenerMonosChinos maraton = filter ((== "Monito chino").genero) maraton

obtenerOriginales maraton = filter (esOriginalDeNetflis) maraton
obtenerOriginalesQueValen maraton = filter (valeLaPena) (obtenerOriginales maraton)

obtenerPorCantDeTempos cantDeTemporadas maraton = filter ((== cantDeTemporadas). cantTemporadas) maraton

esFlojita maraton = obtenerPorCantDeTempos 1 maraton == maraton

cuantoTarda maraton = sum $ map duracion maraton

valeLaPenaMaraton' maraton | elem rompiendoMalo maraton = True
                           | otherwise = elem (True) (map valeLaPena maraton)

calificacionesMaraton [] = []
calificacionesMaraton (m : ms) = calificaciones m ++ calificacionesMaraton ms
calificacionMasAlta maraton = maximum (calificacionesMaraton $ obtenerOriginales maraton)

hypearSiCorresponde [] = []
hypearSiCorresponde (m : ms) | (genero m == "Drama" || genero m == "Suspenso") = (hypearSerie m) : (hypearSiCorresponde ms)
                             | otherwise = m : (hypearSiCorresponde ms) 

--Parte 3

cantDeTemporadasMaraton maraton = length $ map cantTemporadas maraton
promedioDuracionMaraton maraton = div (cuantoTarda maraton) (cantDeTemporadasMaraton maraton)

--calificacionMaraton maraton = div (sum $ calificacionesMaraton maraton) (length $ calificacionesMaraton maraton)

calificacionesListaMaraton [] = []
calificacionesListaMaraton (lista : listas) = calificacionesMaraton lista ++ calificacionesListaMaraton listas
--calificacionListaMaraton lista = div (sum $ calificacionListaMaraton lista) (length $ calificacionListaMaraton lista)

promedioLista funcion lista = div (sum $ funcion lista) (length $ funcion lista)

maximoSegun funcion lista = (head.filter (\elemento -> ((maximum.map funcion) lista) == funcion elemento)) lista

type Calificaciones = [Int]
data Critico = UnCritico {criterio :: (Serie -> Bool), comoCalifica :: (Calificaciones -> Calificaciones)}

serieFloja serie = cantTemporadas serie == 1

dmoleitor = UnCritico {criterio = serieFloja, comoCalifica = reducirCalificaciones2}

reducirCalificaciones2 lista = (filter (>=3) lista) ++ [1]

hypeador = UnCritico {criterio = hypeable, comoCalifica = hypear}

hypeable serie = genero serie == "Drama" || genero serie == "Suspenso"


premiar numero = min 5 (numero + 2)
sinHeadNiTail = tail.init
hypear calificaciones = [(premiar.head) calificaciones] ++ sinHeadNiTail calificaciones ++ [(premiar.last) calificaciones] 

exquisito = UnCritico {criterio = valeLaPena, comoCalifica = calificacionExquisita}

avg lista = div (sum lista) (length lista)
calificacionExquisita calificaciones = [(+1).avg calificaciones ]