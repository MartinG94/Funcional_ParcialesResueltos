{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Fecha = Fecha {
  año :: Int,
  mes :: Int,
  día :: Int
} deriving (Show,Eq)

type Nombre = String

data Invitado = Invitado {
  nombre :: Nombre,
  calendario :: [RestricciónParaJuntarse]
} deriving (Show)

cumpleMiNovia = Fecha 2018 6 28
navidad = Fecha 2018 12 25
sanValentín = Fecha 2019 2 14
aniversario = Fecha 2018 7 11
miCumpleaños = Fecha 2018 7 7
comprarFacturas = Fecha 2018 7 10
díaDeLaIndependencia = Fecha 2018 7 9

-- Punto 1)
-- Tomando el año 2018 como referencia, para cualquier año, los meses 4, 6, 9 y 11 son los que tienen 30 días.
type Mes = Int
type Año = Int
type CantidadDeDías = Int

esBisiesto :: Año -> Bool
esBisiesto año = (mod año 400==0) || (mod año 4==0) && not (mod año 100==0)

cantidadDíasDelMes :: Mes -> Año -> CantidadDeDías
cantidadDíasDelMes 4 _ = 30
cantidadDíasDelMes 6 _ = 30
cantidadDíasDelMes 9 _ = 30
cantidadDíasDelMes 11 _ = 30
cantidadDíasDelMes 2 año
  | esBisiesto año = 29
  | otherwise = 28
cantidadDíasDelMes _ _ = 31

-- Punto 2a)
type RestricciónParaJuntarse = Fecha -> Bool

-- Punto 2b)
impideIr :: RestricciónParaJuntarse -> Fecha -> Bool
impideIr = ($)

estáOcupado :: Invitado -> Fecha -> Bool
estáOcupado invitado unaFecha = (any (flip impideIr unaFecha) . calendario) invitado

estáLibre :: Invitado -> Fecha -> Bool
estáLibre invitado = not . estáOcupado invitado

puedeJuntarse :: Invitado -> [Fecha] -> [Fecha]
puedeJuntarse invitado = filter (estáLibre invitado)

{- Punto 2c)
  ¿Sería posible determinar en qué días puede juntarse un invitado con una cantidad infinita de restricciones? Justificar conceptualmente.
  En este caso, por como está definida la función puedeJuntarse, no. Porque la función nunca terminaría de analizar cada fecha,
  para una lista de reestricciones infinitas.
-}

-- Punto 3a)
tengoUnaCita :: Fecha -> RestricciónParaJuntarse
tengoUnaCita unaFecha = (==) unaFecha

-- Punto 3b)
type DiasFeriados = [Fecha]

esFeriado :: DiasFeriados -> RestricciónParaJuntarse
esFeriado = flip elem

-- Punto 3c)
puedeIrIndeseable :: Invitado -> RestricciónParaJuntarse
puedeIrIndeseable indeseable = estáLibre indeseable

-- Punto 3d)
cuantoFaltaParaQueTermineElMes fecha = (cantidadDíasDelMes (mes fecha) (año fecha)) - día fecha

esFinDeMes :: RestricciónParaJuntarse
esFinDeMes = (<= 5) . cuantoFaltaParaQueTermineElMes

-- Punto 3e)
uhJustoTengoTurnoConElDentista :: RestricciónParaJuntarse
uhJustoTengoTurnoConElDentista _ = True

-- Ejemplos
invitadoInfinito = Invitado "Infi" (repeat (tengoUnaCita cumpleMiNovia))
fechasPrueba = [Fecha 2018 2 17, Fecha 2019 3 24]

nico = Invitado "Nico" [uhJustoTengoTurnoConElDentista]
martín = Invitado "Martín" [tengoUnaCita cumpleMiNovia, esFeriado [navidad, sanValentín, díaDeLaIndependencia], puedeIrIndeseable nico]
edu = Invitado "Eduardo" [esFinDeMes]
santi = Invitado "Santiago" [tengoUnaCita comprarFacturas]

-- Punto 4)
nuevaRestricción unaRestricción invitado = invitado {calendario = unaRestricción}

agregarRestricción fecha invitado
  | estáLibre invitado fecha = nuevaRestricción (tengoUnaCita fecha : calendario invitado) invitado
  | otherwise = invitado

agendarCita :: Fecha -> Invitado -> Invitado
agendarCita unaFecha = agregarRestricción unaFecha

-- Punto 5a)
type Reunión = [Fecha]
type Cantidad = Int

máximoSegún :: (Num b, Ord b) => (a -> b) -> [a] -> a
máximoSegún criterio lista = (fromJust . find (\ unaFecha -> all (\ otraFecha -> criterio unaFecha >= criterio otraFecha) lista )) lista

cuantaGenteSeJunta :: [Invitado] -> Fecha -> Cantidad
cuantaGenteSeJunta losInvitados fecha =
  (length . filter (flip estáLibre fecha)) losInvitados

determinarMejorDía :: Reunión -> [Invitado] -> Fecha
determinarMejorDía diasDisponibles invitados = máximoSegún (cuantaGenteSeJunta invitados) diasDisponibles

-- Punto 5b)
agendarReunión :: Reunión -> [Invitado] -> [Invitado]
agendarReunión unaReunión invitados = map (agendarCita (determinarMejorDía unaReunión invitados)) invitados

-- Punto 6)
maratónDeReuniones :: [Reunión] -> [Invitado] -> [Invitado]
maratónDeReuniones reuniones invitados = foldr agendarReunión invitados reuniones
