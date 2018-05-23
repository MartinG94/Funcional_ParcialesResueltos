{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- Punto 1)
type Nombre = String
type Concepto = String
type Dedicación = Int

data Alumno = Alumno {
  nombreAlumno :: Nombre,
  dedicación :: Dedicación,
  materiasAprobadas :: [Materia],
  conceptosAprendidos :: [Concepto]
} deriving (Show)

-- Funciones Auxiliares Necesarias
nuevoNombre otroNombre alumno = alumno {nombreAlumno = otroNombre}
agregarPrefijo unPrefijo alumno = nuevoNombre (unPrefijo ++ nombreAlumno alumno) alumno

nuevaDedicación otraDedicación alumno = alumno {dedicación = otraDedicación}
aumentarDedicación cantidad alumno = nuevaDedicación (cantidad + dedicación alumno) alumno

nuevoConcepto otroConcepto alumno = alumno {conceptosAprendidos = otroConcepto}
aprenderConcepto unConcepto alumno = nuevoConcepto (unConcepto : conceptosAprendidos alumno) alumno

nuevaMateria otraMateria alumno = alumno {materiasAprobadas = otraMateria}
aprenderMateria unaMateria alumno = nuevaMateria (unaMateria : materiasAprobadas alumno) alumno

eliminarConcepto unConcepto alumno = alumno {conceptosAprendidos = (filter ((/=) unConcepto) . conceptosAprendidos) alumno}

conceptosRecursivos = map ((++) "Recursividad " . show) [1..]
--

type EfectoAlumno = Alumno -> Alumno
type CondiciónParaAprobar = Alumno -> Bool

-- Nuevas Funciones Auxiliares
sabe :: Concepto -> CondiciónParaAprobar
sabe unConcepto alumno = elem unConcepto (conceptosAprendidos alumno)

aprobó :: Nombre -> CondiciónParaAprobar
aprobó nombreDeMateria = any ((== nombreDeMateria) . nombreMateria) . materiasAprobadas
--

-- Punto 2a)
data Materia = Materia {
  nombreMateria :: Nombre,
  efecto :: EfectoAlumno,
  condiciónParaAprobar :: CondiciónParaAprobar
} deriving (Show)

paradigmas = Materia {
  nombreMateria = "Paradigmas",
  efecto = aumentarDedicación 100 . aprenderConcepto "polimorfismo" . aprenderConcepto "ordenSuperior",
  condiciónParaAprobar = sabe "paremetrización"
}

sistemasOperativos reentregas = Materia {
  nombreMateria = "Sistemas Operativos",
  efecto = aumentarDedicación (1000 * reentregas) . agregarPrefijo "Excelentísim@ ",
  condiciónParaAprobar = (\ _ -> reentregas < 5)
}

recursividadFull = Materia {
  nombreMateria = "Recursividad Full",
  efecto = nuevoConcepto conceptosRecursivos,
  condiciónParaAprobar = aprobó "Recursividad Full"
}

desastre2 = Materia {
  nombreMateria = "Desastre 2",
  efecto = eliminarConcepto "polimorfismo",
  condiciónParaAprobar = (\ _ -> True)
}

martin = Alumno "Martín" 0 [] []

-- Punto 2b)
aprobar :: Materia -> Alumno -> Alumno
aprobar materia = aprenderMateria materia . efecto materia

-- Punto 6)
aprobarSiPuede :: Materia -> Alumno -> Alumno
aprobarSiPuede materia alumno
  | condiciónParaAprobar materia alumno = aprobar materia alumno
  | otherwise = error "No se puede aprobar!"

-- Punto 4)
type Cursada = [Materia]

aprobarCursada :: Cursada -> Alumno -> Alumno
aprobarCursada cursada alumno = foldr aprobar alumno cursada

-- Punto 5)
type Puntaje = Int
type Criterio = Alumno -> Puntaje

cantidadDeConceptos :: Criterio
cantidadDeConceptos = length . conceptosAprendidos

menosHorasDeDedicación :: Criterio
menosHorasDeDedicación = (*) (-1) . dedicación

cantidadLetrasPrimerConcepto :: Criterio
cantidadLetrasPrimerConcepto = length . head . conceptosAprendidos

puntajeLuegoDeCursar :: Criterio -> Cursada -> Alumno -> Puntaje
puntajeLuegoDeCursar unCriterio cursada = unCriterio . aprobarCursada cursada

esMejorSegún :: Criterio -> Cursada -> Cursada -> Alumno -> Bool
esMejorSegún unCriterio cursadaBuena cursadaMala alumno =
  puntajeLuegoDeCursar unCriterio cursadaBuena alumno > puntajeLuegoDeCursar unCriterio cursadaMala alumno

-- Punto 6) Incluido en la resolución

{-
¿Será posible determinar si una cursada que incluye la materia “Recursividad a full” es mejor que otra para un alumno?
Justificar conceptualmente analizando los distintos criterios de comparación.

Sería posible ver si es mejor una cursada que incluye Recursividad a Full,
si es que comparamos por dedicación o si comparamos por los caracteres del primer concepto aprendido,
ya que no necesitamos que se termine de generar la lista infinita de conceptos que hace que aprenda.

En cambio, si comparamos por cantidad de conceptos aprendidos, ahí se cuelga
porque nunca termina de terminar la lista como para saber su cantidad total.

Ambos casos, son gracias a la evaluación diferida, ya que sin ella,
no se podría ni siquiera empezar a evaluar algo con una entidad que aún no se terminó de construir y traer entera a memoria.
-}
