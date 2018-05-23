{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Nombre = String
type Sentimiento = String
type Estrategia = Persona -> Persona
data Persona = Vendedor Nombre Estrategia [Sentimiento] | Comprador Nombre [Sentimiento] deriving Show

-- Funciones Auxiliares necesarias [Polimorfismo Ad-Hoc]; Se usa Pattern Matching
nombre (Vendedor nombre _ _) = nombre
nombre (Comprador nombre _) = nombre

sentimientos (Vendedor _ _ sentimientos) = sentimientos
sentimientos (Comprador _ sentimientos) = sentimientos

estrategia (Vendedor _ estrategia _) = estrategia

nuevoNombre otroNombre (Vendedor nombre estrategia sentimientos) = Vendedor otroNombre estrategia sentimientos
nuevoNombre otroNombre (Comprador nombre sentimientos) = Comprador otroNombre sentimientos

nuevoSentimiento otroSentimiento (Vendedor nombre estrategia sentimientos) = Vendedor nombre estrategia (otroSentimiento : sentimientos)
nuevoSentimiento otroSentimiento (Comprador nombre sentimientos) = Comprador nombre (otroSentimiento : sentimientos)

nuevaEstrategia otraEstrategia (Vendedor nombre estrategia sentimientos) = Vendedor nombre otraEstrategia sentimientos
--

jorgito = Comprador "Jorge" ["molestia"]
tincho = Comprador "Martín" ["indiferencia", "molestia"]
agus = Comprador "Agustín" ["felicidad"]
loquito = Comprador "Loquito" ["molestia", "bronca", "ira asesina"]

lucas = Vendedor "Lucas" gauchada ["felicidad"]
pato = Vendedor "Patricio" (fraudeOlímpico . niFuNiFa) ["bronca", "felicidad"]
flor = Vendedor "Florencia" (estafa . fraudeOlímpico) []
nacho = Vendedor "Ignacio" (estafa . seguidilla 10 . niFuNiFa) ["molestia"]

-- Polimorfismo Paramétrico
-- Punto 1a)
sentir :: Sentimiento -> Persona -> Persona
sentir unSentimiento persona = nuevoSentimiento unSentimiento persona

niFuNiFa :: Estrategia
niFuNiFa = id

-- Punto 1b)
gauchada :: Estrategia
gauchada persona = sentir "satisfacción" persona

-- Punto 1c)
estafa :: Estrategia
estafa persona = (sentir "bronca" . sentir "felicidad") persona

fraudeOlímpico :: Estrategia
fraudeOlímpico persona = (sentir "ira asesina" . estafa) persona

-- Punto 1d)
type Cantidad = Int

seguidilla :: Cantidad -> Estrategia
seguidilla cantidad persona = foldr sentir persona (replicate cantidad  "molestia")

-- Punto 2a)
sentimientosMalos = ["molestia", "bronca", "ira asesina"]

esMalo unSentimiento = elem unSentimiento sentimientosMalos
tieneMalosSentimientos lista = any esMalo lista

seSienteBien :: Persona -> Bool
seSienteBien persona = (not . tieneMalosSentimientos) (sentimientos persona)

-- Punto 2b)
seSienteMal :: Persona -> Bool
seSienteMal persona = tieneMalosSentimientos (sentimientos persona)

-- Función Auxiliar proporcionada
elementosÚnicos [] = []
elementosÚnicos (x:xs)
   | (not . elem x) xs = x : elementosÚnicos xs
   | otherwise = elementosÚnicos xs

-- Punto 2c)
cantidadDeSentimientosDiferentes = length . elementosÚnicos . sentimientos
másDeNSentimientosDiferentes cantidad = (== cantidad) . cantidadDeSentimientosDiferentes
contiene sentimiento = elem sentimiento . sentimientos

quiereMatarATodos :: Persona -> Bool
quiereMatarATodos persona = seSienteMal persona && másDeNSentimientosDiferentes 3 persona && contiene "ira asesina" persona

-- Funciones Proporcionadas
agradecimiento :: Estrategia
agradecimiento = sentir "placer"

defensaAlConsumidor :: Estrategia
defensaAlConsumidor = sentir "tristeza" . sentir "miedo"

juicio :: Cantidad -> Estrategia
juicio cantidadDeAbogados = sentir "depresión" . seguidilla cantidadDeAbogados

-- Punto 3)
cantidadAbogadosQueContrata = length . sentimientos

reaccionar :: Persona -> Persona -> Estrategia
reaccionar unComprador unVendedor
    | (quiereMatarATodos . estrategia unVendedor) unComprador = juicio (cantidadAbogadosQueContrata unComprador)
    | (seSienteBien . estrategia unVendedor) unComprador = agradecimiento
    | otherwise = defensaAlConsumidor

-- Punto 4)
primerSentimientoLuegoDeLaVenta :: Sentimiento -> Persona -> Persona -> Bool
primerSentimientoLuegoDeLaVenta sentimiento unComprador unVendedor =
  ((== sentimiento) . head . sentimientos . reaccionar unComprador unVendedor) unVendedor

-- Punto 4a)
sientePlacerLuegoDeLaVenta :: Persona -> Persona -> Bool
sientePlacerLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "placer"

-- Punto 4b)
sienteDepresiónLuegoDeLaVenta :: Persona -> Persona -> Bool
sienteDepresiónLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "depresión"

-- Punto 4c)
sienteTristezaLuegoDeLaVenta :: Persona -> Persona -> Bool
sienteTristezaLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "tristeza"

-- Punto 4d)
aplicarReacción comprador vendedor = reaccionar comprador vendedor vendedor

ventaÉlite :: Persona -> [Persona] -> [Persona]
ventaÉlite comprador listaVendedores = (map (aplicarReacción comprador) . filter (sientePlacerLuegoDeLaVenta comprador)) listaVendedores
