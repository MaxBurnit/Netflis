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
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

--Parte 1
maraton = [tioGolpetazo, dbs, rompiendoMalo]
cantSeries = length maraton 
cantCalificaciones serie = length (calificaciones serie)
--obtenerBreakingBad maraton = elem 
esPopular serie | cantCalificaciones serie >= 3 = "Es popular"
                | otherwise = "No es popular"
valeLaPena serie | (cantTemporadas serie > 1) && (esPopular serie == "Es popular") = "Vale la pena"
                 | otherwise = "No vale la pena"
--valeLaPenaMaraton maraton = ((valeLaPena.head maraton == "Vale la pena") && (valeLaPena.last maraton == "Vale la pena"))

--Parte3
data Critico = UnCritico {
	criterio :: (Serie -> Bool),
	comoCalifica :: (Calificaciones -> Calificaciones)
}
duracionSerie serie = cantTemporadas serie * duracion serie
--promedioDuracion maraton = div (sum(map duracionSerie maraton))  (length maraton)
--calificacionFinal maraton = div (sum(map calificacion maraton)) (length maraton)
promedio f maraton = div (sum(map f maraton)) (length maraton)
dMoleitor maraton = map criticar maraton
criticar criterio evaluacion serie | criterio serie = evaluacion serie
                                                | otherwise = serie

