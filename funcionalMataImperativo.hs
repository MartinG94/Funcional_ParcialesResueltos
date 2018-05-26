{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- Punto 1a)
type Nombre = String
type Dólares = Double
type Cantidad = Int
type PoblaciónActiva = Cantidad
type PuestosDeTrabajo = PoblaciónActiva
type Recurso = String

data País = País {
  nombrePaís :: Nombre,
  ingresoPerCápita :: Dólares,
  sectorPúblico :: PoblaciónActiva,
  sectorPrivado :: PoblaciónActiva,
  recursos :: [Recurso],
  deuda :: Dólares
} deriving (Show, Eq)

nuevoIPC otroIPC país = país {ingresoPerCápita = otroIPC}
nuevoSectorPúblico otroSectorPúblico país = país {sectorPúblico = otroSectorPúblico}
nuevoSectorPrivado otroSectorPrivado país = país {sectorPrivado = otroSectorPrivado}
nuevosRecursos otrasRecursos país = país {recursos = otrasRecursos}
agregarRecurso unRecurso país = nuevosRecursos (unRecurso : recursos país) país
nuevaDeuda otraDeuda país = país {deuda = otraDeuda}

-- Punto 1b)
niamibia = País "Niamibia" 4140 400000 650000 ["minería", "ecoturismo"] 50000000

-- Punto 2)
type Estrategia = País -> País

calcularInterés :: Dólares -> Dólares
calcularInterés = (* 1.5)

prestarle :: Dólares -> Estrategia
prestarle cantidad país = nuevaDeuda ( deuda país + calcularInterés cantidad ) país

cuantoDisminuyeSegún :: País -> Dólares
cuantoDisminuyeSegún país
  | ((< 100) . sectorPúblico) país = ((*0.8) . ingresoPerCápita) país
  | otherwise = ((*0.85) . ingresoPerCápita) país

reducir :: PuestosDeTrabajo -> Estrategia
reducir trabajo país = (nuevoIPC ( cuantoDisminuyeSegún país ) . nuevoSectorPúblico ( sectorPúblico país - trabajo )) país

embargarRecurso elRecurso = filter (\ recurso -> recurso /= elRecurso)

obtenerRecurso :: Recurso -> Estrategia
obtenerRecurso unRecurso país =
  (nuevaDeuda (deuda país - 2000000) . nuevosRecursos (embargarRecurso unRecurso (recursos país))) país

calcularPBI país = ingresoPerCápita país * (fromIntegral (sectorPúblico país + sectorPrivado país))

blindaje :: Estrategia
blindaje país = nuevaDeuda ( calcularPBI país / 2 ) país

-- Punto 3a)
type Receta = [Estrategia]
receta1 = [prestarle 200000000, obtenerRecurso "minería"]
receta2 = [prestarle 10000000, reducir 200000, blindaje, obtenerRecurso "petroleo"]

-- Punto 3b)
aplicarReceta :: Receta -> País -> País
aplicarReceta unaReceta país = foldr ($) país unaReceta

-- Punto 4a)
puedenZafarSegún :: Recurso -> [País] -> [País]
puedenZafarSegún unRecurso = filter (\ país -> (elem unRecurso . recursos) país)

zafanLosQueTienenPetroleo :: [País] -> [País]
zafanLosQueTienenPetroleo = puedenZafarSegún "petróleo"

-- Punto 4b)
deudaTotal :: [País] -> Dólares
deudaTotal = sum . map deuda

{- Punto 4c)
Orden Superior: Se usa cuando se utiliza la función "recursos" dentro del filter de puedenZafarSegún
Composición: Se usa en las dos funciones puedenZafarSegún y deudaTotal.
Aplicación Parcial: Se usa con (elem unRecurso . recursos) que espera recibir un país,
y en (sum . map deuda) que espera recibir una lista de paises
-}

-- Punto 5)
conjuntoRecetas = [receta1, receta2]

estáOrdenada :: País -> [Receta] -> Bool
estáOrdenada país (unaReceta : otraReceta : lasRecetas) =
  (calcularPBI . aplicarReceta unaReceta) país < (calcularPBI . aplicarReceta otraReceta) país &&
  estáOrdenada país (otraReceta : lasRecetas)

{- Punto 6)
Si un pais tiene infinitos recursos naturales, modelados con esta función:
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "energia" : recursosNaturalesInfinitos

6a) Que sucede si la evaluamos con la función del punto 4a.
Se cuelga.

6b) Y con 4b.
Puede calcular la deuda sin problemas.
-}
