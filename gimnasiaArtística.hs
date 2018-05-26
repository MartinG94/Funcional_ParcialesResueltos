{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Nivel = Int
type Nombre = String

-- Punto 1)
data Gimnasta = Gimnasta {
  nombre :: Nombre,
  energía :: Nivel,
  equilibrio :: Nivel,
  flexibilidad :: Nivel,
  fuerza :: Nivel,
  habilidades :: [Habilidad]
} deriving (Show)

nuevoNombre otroNombre gimnasta = gimnasta {nombre = otroNombre}
nuevaEnergía otraEnergía gimnasta = gimnasta {energía = otraEnergía}
nuevoEquilibrio otroEquilibrio gimnasta = gimnasta {equilibrio = otroEquilibrio}
nuevaFlex otraFlexibilidad gimnasta = gimnasta {flexibilidad = otraFlexibilidad}
nuevaFuerza otraFuerza gimnasta = gimnasta {fuerza = otraFuerza}
nuevasHabilidades otrasHabilidades gimnasta = gimnasta {habilidades = otrasHabilidades}
agregarHabilidad unaHabilidad gimnasta = nuevasHabilidades (unaHabilidad : habilidades gimnasta) gimnasta

laMitad = (flip div 2)

type Habilidad = Gimnasta -> Gimnasta

medialuna :: Habilidad
medialuna gimnasta = nuevoEquilibrio (equilibrio gimnasta + 5) gimnasta

type Velocidad = Int

rolAdelante :: Velocidad -> Habilidad
rolAdelante velocidad gimnasta = nuevaEnergía (energía gimnasta + laMitad velocidad) gimnasta

vertical :: Habilidad
vertical gimnasta = nuevaFuerza (fuerza gimnasta + 7) gimnasta

type Cantidad = Int

saltoConSoga :: Cantidad -> Habilidad
saltoConSoga saltos gimnasta =
  (nuevaEnergía (energía gimnasta - laMitad saltos) . nuevaFuerza (fuerza gimnasta + saltos)) gimnasta

type Altura = Int
type Impulso = Int

saltoMortal :: Altura -> Impulso -> Habilidad
saltoMortal altura impulso gimnasta =
  (nuevaFuerza (fuerza gimnasta + altura) . nuevaFlex (flexibilidad gimnasta + laMitad impulso)) gimnasta

-- Punto 2)
sonia = Gimnasta "Sonia" 90 60 40 50 [medialuna, rolAdelante 20, saltoMortal 40 50]
pedro = Gimnasta "Pedro" 70 50 50 60 [saltoConSoga 150, vertical, rolAdelante 30]

-- Punto 3a)
componerHabilidades = foldl (flip (.)) id

type Minutos = Int

realizar :: Minutos -> Habilidad -> Gimnasta -> Gimnasta
realizar cantidad habilidad = componerHabilidades (replicate (laMitad cantidad) habilidad)

ejercitar :: Minutos -> Habilidad -> Gimnasta -> Gimnasta
ejercitar minutos habilidad = agregarHabilidad habilidad . realizar minutos habilidad

-- Punto 3b)
data Rutina = Rutina {
  repeticiones :: Cantidad,
  ejercicios :: [Habilidad]
} deriving (Show)

entradaEnCalor = Rutina {
  repeticiones = 2,
  ejercicios = [rolAdelante 10, rolAdelante 10 ,medialuna ,medialuna ,medialuna ,medialuna ,saltoConSoga 50 ,saltoMortal 20 15]
}

rutinaDiaria = Rutina {
  repeticiones = 3,
  ejercicios = [rolAdelante 20, saltoConSoga 30, vertical, medialuna, saltoConSoga 10]
}

-- Punto 3c)
entrenar :: Rutina -> Gimnasta -> Gimnasta
entrenar unaRutina = componerHabilidades (concat (replicate (repeticiones unaRutina) (ejercicios unaRutina)))

-- Punto 3d)
hacerSusEjercicios :: Gimnasta -> Gimnasta
hacerSusEjercicios gimnasta = (componerHabilidades (habilidades gimnasta)) gimnasta

fortaleza :: Gimnasta -> Nivel
fortaleza gimnasta = fuerza gimnasta + energía gimnasta

fortalezaMayorA :: Nivel -> Gimnasta -> Bool
fortalezaMayorA nivel gimnasta = fortaleza gimnasta > nivel

tienenPotencial :: Nivel -> [Gimnasta] -> [Gimnasta]
tienenPotencial nivel = filter (\ unGimnasta -> (fortalezaMayorA nivel . entrenar rutinaDiaria . hacerSusEjercicios) unGimnasta )

-- Punto 4)
máximoSegún :: (Num b, Ord b) => (a -> b) -> [a] -> a
máximoSegún criterio lista = (fromJust . find (\ e1 -> all (\ e2 -> criterio e1 >= criterio e2) lista)) lista

mínimoSegún :: (Num b, Ord b) => (a -> b) -> [a] -> a
mínimoSegún criterio = máximoSegún ((*) (-1) . criterio)

-- Punto 4.a)
quienTieneMásFuerza :: [Gimnasta] -> Gimnasta
quienTieneMásFuerza = máximoSegún fuerza

-- Punto 4.b)
aguante :: Gimnasta -> Nivel
aguante gimnasta = flexibilidad gimnasta + fortaleza gimnasta

quienTieneMenosAguante :: [Gimnasta] -> Gimnasta
quienTieneMenosAguante = mínimoSegún aguante

-- Punto 4.c)
cantidadDeHabilidadesLuegoDe :: Habilidad -> Gimnasta -> Cantidad
cantidadDeHabilidadesLuegoDe unaHabilidad = length . habilidades . ejercitar 10 unaHabilidad

quienTieneMasEjerciciosDespuésDe :: Habilidad -> [Gimnasta] -> Gimnasta
quienTieneMasEjerciciosDespuésDe unaHabilidad = máximoSegún (cantidadDeHabilidadesLuegoDe unaHabilidad)
