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

nuevoNombre otroNombre (Vendedor nombre estrategia [sentimiento]) = Vendedor otroNombre estrategia [sentimiento]
nuevoNombre otroNombre (Comprador nombre [sentimiento]) = Comprador otroNombre [sentimiento]

nuevoSentimiento otroSentimiento (Vendedor nombre estrategia lista) = Vendedor nombre estrategia (otroSentimiento : lista)
nuevoSentimiento otroSentimiento (Comprador nombre lista) = Comprador nombre (otroSentimiento : lista)

nuevaEstrategia otraEstrategia (Vendedor nombre estrategia [sentimiento]) = Vendedor nombre otraEstrategia [sentimiento]
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
gauchada persona = nuevoSentimiento "satisfacción" persona

-- Punto 1c)
estafa :: Estrategia
estafa persona = (nuevoSentimiento "bronca" . nuevoSentimiento "felicidad") persona

fraudeOlímpico :: Estrategia
fraudeOlímpico persona = (nuevoSentimiento "ira asesina" . estafa) persona

-- Punto 1d)
type Cantidad = Int

seguidilla :: Cantidad -> Estrategia
seguidilla cantidad persona = foldr sentir persona (replicate cantidad  "molestia")

sentimientosMalos = ["molestia", "bronca", "ira asesina"]

esMalo unSentimiento = elem unSentimiento sentimientosMalos
tieneMalosSentimientos lista = any esMalo lista

-- Punto 2a)
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

cantidadDeSentimientosDiferentes = length . elementosÚnicos . sentimientos
másDeNSentimientosDiferentes cantidad = (== cantidad) . cantidadDeSentimientosDiferentes
contiene sentimiento = elem sentimiento . sentimientos

-- Punto 2c)
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
reaccionar :: Persona -> Persona -> Estrategia
reaccionar unComprador unVendedor
    | (quiereMatarATodos . estrategia unVendedor) unComprador = juicio ((length . sentimientos) unComprador)
    | (seSienteBien . estrategia unVendedor) unComprador = agradecimiento
    | otherwise = defensaAlConsumidor

-- Punto 4)
primerSentimientoLuegoDeLaVenta :: Sentimiento -> Persona -> Persona -> Bool
primerSentimientoLuegoDeLaVenta sentimiento unComprador unVendedor = ((== sentimiento) . head . sentimientos . reaccionar unComprador unVendedor) unVendedor

-- Punto 4a)
sientePlacerLuegoDeLaVenta :: Persona -> Persona -> Bool
sientePlacerLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "placer"

-- Punto 4b)
sienteDepresiónLuegoDeLaVenta :: Persona -> Persona -> Bool
sienteDepresiónLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "depresión"

-- Punto 4c)
sienteTristezaLuegoDeLaVenta :: Persona -> Persona -> Bool
sienteTristezaLuegoDeLaVenta = primerSentimientoLuegoDeLaVenta "tristeza"

aplicarReacción comprador vendedor = reaccionar comprador vendedor vendedor

-- Punto 4d)
ventaÉlite :: Persona -> [Persona] -> [Persona]
ventaÉlite comprador listaVendedores = (map (aplicarReacción comprador) . filter (sientePlacerLuegoDeLaVenta comprador)) listaVendedores
