{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- Parte A

-- Punto 1)
type Enfermedad = String
data Sexo = Femenino | Masculino deriving (Show, Eq)
type Peso = Float
type Edad = Int

data Socio = Socio {
  sexo :: Sexo,
  edad :: Edad,
  peso :: Peso,
  preexistencia :: [Enfermedad]
} deriving (Show, Eq)

type Dinero = Float
type Cantidad = Int

data Tratamiento = Tratamiento {
  costoBase :: Dinero,
  sesiones :: Cantidad,
  enfermedad :: Enfermedad
} deriving (Show, Eq)

josé = Socio Masculino 22 78.92 ["zamorreaDistópica"]
analía = Socio Femenino 34 70 []

x1 = Tratamiento 5000 30 "zamorreaDistópica"
xfg23 = Tratamiento 10000 2 "zamorreaDistópica"

nuevaEnfermedad unaEnfermedad socio = socio {preexistencia = unaEnfermedad}
agregarPreexistencia otraEnfermedad socio = nuevaEnfermedad (otraEnfermedad : preexistencia socio) socio

-- Punto 2)
diagnosticarPreexistencia :: Enfermedad -> Socio -> Socio
diagnosticarPreexistencia = agregarPreexistencia

esObeso = (> 150) . peso
tieneEdadAvanzada = (> 75) . edad
tieneMuchasPreexistencias = (> 8) . length . preexistencia

-- Punto 3)
estáEnRiesgo :: Socio -> Bool
estáEnRiesgo socio = esObeso socio || tieneEdadAvanzada socio || tieneMuchasPreexistencias socio

-- Parte B

-- Punto 4)
data Solicitud = Solicitud {
  socio:: Socio,
  tratamiento::Tratamiento
} deriving (Show, Eq)

solicitud897 = Solicitud josé x1

type Cobertura = Dinero
type Prestación = Solicitud -> Cobertura

costoDeSolicitud = costoBase . tratamiento
enfermedadDeLaSolicitud = enfermedad . tratamiento

-- Punto 5a)
prestaciónTotal :: Enfermedad -> Prestación
prestaciónTotal unaEnfermedad solicitud
  | ((== unaEnfermedad) . enfermedadDeLaSolicitud) solicitud = costoDeSolicitud solicitud
  | otherwise = 0

laEnfermedadEsPreexistente unaEnfermedad = elem unaEnfermedad . preexistencia
laEnfermedadNoEsPreexistente unaEnfermedad = not . laEnfermedadEsPreexistente unaEnfermedad

-- Punto 5b)
prestaciónSinPreexistencias :: Enfermedad -> Prestación
prestaciónSinPreexistencias unaEnfermedad solicitud
  | laEnfermedadNoEsPreexistente (enfermedadDeLaSolicitud solicitud) (socio solicitud) = ((*0.5) . costoDeSolicitud) solicitud897
  | otherwise = 0

-- Punto 5c)
prestaciónHastaMáximo :: Dinero -> Prestación
prestaciónHastaMáximo cantidadQueCubre = min cantidadQueCubre . costoDeSolicitud

-- Punto 5d)
nada :: Prestación
nada _ = 0

-- Punto 6a)
sumarPrestaciones :: Prestación -> Prestación -> Prestación
sumarPrestaciones unaPrestación otraPrestación solicitud = max (unaPrestación solicitud) (otraPrestación solicitud)

-- Punto 6b)
plan :: [Prestación] -> Prestación
plan = foldr sumarPrestaciones nada
