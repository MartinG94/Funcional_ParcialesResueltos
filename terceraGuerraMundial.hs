{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- Punto 1)
type Nombre = String
type Puntaje = Double
type Estrategia = País -> País

data País = País {
  nombre :: Nombre,
  recursos :: Puntaje,
  enemigos :: [País],
  aliados :: [País],
  estrategia :: Estrategia
} deriving (Show)

-- Auxiliares
nuevosRecursos otrosRecursos país = país {recursos = max 0 otrosRecursos}

nuevosEnemigos otrosEnemigos país = país {enemigos = otrosEnemigos}
agregarEnemigo unEnemigo país = nuevosEnemigos (unEnemigo : enemigos país) país

nuevosAliados otrosAliados país = país {aliados = otrosAliados}
agregarAliado unAliado país = nuevosAliados (unAliado : aliados país) país
--

estadosUnidos = País {
  nombre = "Estados Unidos",
  recursos = 1500,
  enemigos = [afganistán, coreaDelNorte, china],
  aliados = [alemania, inglaterra],
  estrategia = lanzarBombas 3 . influenciar
}

rusia = País {
  nombre = "Rusia",
  recursos = 2100,
  enemigos = [turquía, estadosUnidos],
  aliados = [],
  estrategia = lanzarBombas 10 . complot . esparcirVirus "Witzelsucht"
}

paísGenérico = País "" 1000 [] [] (lanzarBombas 1)

afganistán = paísGenérico {nombre = "Afganistán"}
coreaDelNorte = paísGenérico {nombre = "Corea Del Norte"}
china = paísGenérico {nombre = "China"}
alemania = paísGenérico {nombre = "alemania"}
inglaterra = paísGenérico {nombre = "inglaterra"}
turquía = paísGenérico {nombre = "Turquía"}

-- Punto 2)
influenciar :: Estrategia
influenciar país = nuevosAliados [] país

afectarRecursosCon criterio país = nuevosRecursos ((criterio . recursos) país) país

type Cantidad = Double

lanzarBombas :: Cantidad  -> Estrategia
lanzarBombas nroBombas país = afectarRecursosCon (* ((1-0.1)*nroBombas)) país

type Virus = String

esparcirVirus :: Virus -> Estrategia
esparcirVirus virus país
  | ((>20).length) virus = afectarRecursosCon ((+) (-100)) país
  | otherwise = nuevosRecursos 0 país

nuevosEnemigosSegún unCriterio país = nuevosEnemigos (map unCriterio (enemigos país)) país
potenciar = afectarRecursosCon (+1000)

complot :: Estrategia
complot país = nuevosEnemigosSegún potenciar país

-- Punto 3)
atacarAEnemigos país = nuevosEnemigosSegún (estrategia país) país
contrataqueDeEnemigos país = (foldr (estrategia) país . enemigos) país

resultado :: País -> País
resultado = contrataqueDeEnemigos . atacarAEnemigos

-- Punto 4)
recursosAceptables = (>=500) . recursos
tieneMásRecursos país = (< recursos país)
tieneMásRecursosQueSusEnemigos país = all (tieneMásRecursos país) (map recursos (enemigos país))

losGanadores país =
  (recursosAceptables . resultado) país &&
  (tieneMásRecursosQueSusEnemigos . resultado) país

análisis :: [País] -> [País]
análisis paises = filter (losGanadores) paises

cómoQuedaronGanadores :: [País] -> [País]
cómoQuedaronGanadores = filter losGanadores . map resultado


{- Punto 5)
Orden superior: Se usó al invocar funciones como map, que le mandamos por parámetro otra función.
Esto hace que la lógica del map la podamos reutilizar para muchos casos sin tener que redefinirla en otras funciones.
Lo mismo pasa en la función afectarRecursosCon y nuevosEnemigosSegún, que son muy genéricas gracias a recibir una función por parámetro.

Aplicación parcial: Es cuando se envían menos argumentos a una función de los que necesita, y eso devuelve una nueva función
que espera los parámetros restantes. Por ejemplo, en nuevosEnemigosSegún, que escribimos “map potenciar”, y eso es una nueva función
que resulta de aplicar parcialmente a map. O en “bombas 3”, y otros casos más.
Esto, al igual que el orden superior, también permite crear nueva lógica fácilmente (nuevas funciones, nuevo comportamiento)
sin tener que codificar cosas nuevas.

Composición: Es cuando generamos una nueva función a partir de combinar otras dos,
haciendo que el resultado de una se le envíe como entrada a otra.
De esta forma, creamos nuevas funcionalidades a partir de combinar otras sin tener que reescribir esas partes de la lógica.
No codificamos nada nuevo, sino que combinamos fácilmente lo existente para hacer una nueva funcionalidad.
Se usó, por ejemplo, en cómoQuedaronGanadores, en donde componemos el filter parcialmente aplicado, con el map también parcialmente aplicado,
haciendo que el resultado de ese map (una lista) se le envíe a ese filter (que espera recibir una lista).
-}
