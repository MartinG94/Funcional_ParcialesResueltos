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
  calendario :: [ReestricciónParaJuntarse]
} deriving (Show)

cantidadDíasDelMes _ = 31

cumpleMiNovia = Fecha 2018 6 28
navidad = Fecha 2018 12 25
sanValentín = Fecha 2019 2 14

-- Punto 1a)
type ReestricciónParaJuntarse = Fecha -> Bool

-- Punto 1b)
estáOcupado :: Invitado -> Fecha -> Bool
estáOcupado invitado unaFecha = (any (\ reestricción -> reestricción unaFecha) . calendario) invitado

estáLibre :: Invitado -> Fecha -> Bool
estáLibre invitado = not . estáOcupado invitado

puedeJuntarse :: Invitado -> [Fecha] -> [Fecha]
puedeJuntarse invitado = filter (estáLibre invitado)

-- Punto 2)
tengoUnaCita :: Fecha -> ReestricciónParaJuntarse
tengoUnaCita unaFecha = (==) unaFecha

esFeriado :: [Fecha] -> ReestricciónParaJuntarse
esFeriado fechas = (flip elem) fechas

puedeIrIndeseable :: Invitado -> ReestricciónParaJuntarse
puedeIrIndeseable indeseable = estáLibre indeseable

esFinDeMes :: ReestricciónParaJuntarse
esFinDeMes fecha = ((cantidadDíasDelMes . mes) fecha - día fecha) < 5

uhJustoTengoTurnoConElDentista :: ReestricciónParaJuntarse
uhJustoTengoTurnoConElDentista _ = True

nico = Invitado "Nico" [uhJustoTengoTurnoConElDentista]
martín = Invitado "Martín" [tengoUnaCita cumpleMiNovia, esFeriado [navidad, sanValentín], puedeIrIndeseable nico]
edu = Invitado "Eduardo" [esFinDeMes]

-- Punto 3)

nuevaReestricción unaReestricción invitado = invitado {calendario = unaReestricción}

agregarReestricción fecha invitado
  | estáLibre invitado fecha = nuevaReestricción (tengoUnaCita fecha : calendario invitado) invitado
  | otherwise = invitado

agendarCita :: Fecha -> Invitado -> Invitado
agendarCita unaFecha = agregarReestricción unaFecha

-- Punto 4a)
type Reunión = [Fecha]
type Cantidad = Int

máximoSegún :: (Num b, Ord b) => (a -> b) -> [a] -> a
máximoSegún criterio lista = (fromJust . find (\ unaFecha -> all (\ otraFecha -> criterio unaFecha >= criterio otraFecha) lista )) lista

cuantaGenteSeJunta :: [Invitado] -> Fecha -> Cantidad
cuantaGenteSeJunta losInvitados fecha =
  (length . filter (\ unInvitado -> estáLibre unInvitado fecha)) losInvitados

determinarMejorDía :: Reunión -> [Invitado] -> Fecha
determinarMejorDía diasDisponibles invitados = máximoSegún (cuantaGenteSeJunta invitados) diasDisponibles

-- Punto 4b)
agendarReunión :: Reunión -> [Invitado] -> [Invitado]
agendarReunión unaReunión invitados = map (agendarCita (determinarMejorDía unaReunión invitados)) invitados

-- Punto 5)
maratónDeReuniones :: [Reunión] -> [Invitado] -> [Invitado]
maratónDeReuniones reuniones invitados = foldr agendarReunión invitados reuniones
