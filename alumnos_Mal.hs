{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Nombre = String
type Dedicación = Int
type Materia = Alumno -> Alumno -- Mal Planteado -> "De la materia nos interesa su NOMBRE y EFECTO sobre el alumno"
type Concepto = String

data Alumno = Alumno {
  nombre::Nombre,
  dedicación::Dedicación,
  materias::[Materia],
  conceptos::[Concepto]
} deriving (Show)

martin = Alumno "Martín" 0 [] []

nuevoNombre otroNombre alumno = alumno {nombre = otroNombre}
nuevaDedicación otraDedicación alumno = alumno {dedicación = otraDedicación}
nuevaMateria otraMateria alumno = alumno {materias = otraMateria : materias alumno}
nuevoConcepto unConcepto alumno = alumno {conceptos = unConcepto}
agregarConcepto otroConcepto alumno = nuevoConcepto (otroConcepto : conceptos alumno) alumno

paradigmas :: Materia
paradigmas alumno =
  (nuevaDedicación (((+100).dedicación) alumno) . agregarConcepto "polimorfismo" . agregarConcepto "ordenSuperior" ) alumno

type Cantidad = Int

sistemasOperativos :: Cantidad -> Materia
sistemasOperativos reentregas alumno =
  (nuevaDedicación ((*1000) reentregas + dedicación alumno) . nuevoNombre ("Excelentísim@ " ++ nombre alumno)) alumno

repetirConcepto :: Concepto -> Int -> [Concepto]
repetirConcepto concepto desde = (concepto ++ show desde) : repetirConcepto concepto (desde + 1)

recursividadFull :: Materia
recursividadFull alumno = nuevoConcepto (repetirConcepto "Recursividad" 1) alumno

eliminarConcepto :: Concepto -> [Concepto] -> [Concepto]
eliminarConcepto concepto = filter (\ elemento -> concepto /= elemento)

desastre2 :: Materia
desastre2 alumno = nuevoConcepto (eliminarConcepto "polimorfismo" (conceptos alumno)) alumno

aprobar :: Materia -> Alumno -> Alumno
aprobar unaMateria alumno = nuevaMateria unaMateria (unaMateria alumno)

type Cursada = [Materia]

aprobarCursada :: Cursada -> Alumno -> Alumno
aprobarCursada unaCursada alumno = foldr aprobar alumno unaCursada

type Criterio = Alumno -> Int

cantidadConceptos = length . conceptos
menosHorasDeDedicación = (*) (-1) . dedicación
cantidadLetrasPrimerConcepto = length . head . conceptos

puntosSegún unCriterio cursada = unCriterio . aprobarCursada cursada

esMejorSegún :: Criterio -> Cursada -> Cursada -> Alumno -> Bool
esMejorSegún criterio cursadaBuena cursadaMala alumno =
  puntosSegún criterio cursadaBuena alumno > puntosSegún criterio cursadaMala alumno
