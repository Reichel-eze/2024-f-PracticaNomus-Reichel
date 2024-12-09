module Library where
import PdePreludat
import Control.Category (Category)

-- PARTE 1
data Nomu = UnNomu {
    tieneAlas :: Bool,
    tieneMultiplesBrazos :: Bool,
    cantidadOjos :: Number,
    colorDePiel :: ColorDePiel,
    cantVida :: Number,
    fuerza :: Number,
    poderes :: [Poder]  -- los nomus pueden tener muchos poderes (una lista de poderes)
}deriving(Show)

-- Los nomus tiene algunos colores definidos 
-- (los defino de esta manera asi no son stings y no hay ambiguedades)
-- MULTIPLES CONSTRUCTORES (es una especie de Bool pero con mas opciones)
-- Arranca con mayusculas

data ColorDePiel = Gris | Azul | Negro | Verde | Blanco deriving(Show) 

-- Luego se nos pide averiguar si puede ver, es decir, si tiene ojos y su categoría.

puedeVer :: Nomu -> Bool
puedeVer nomu = cantidadOjos nomu > 0

categoria :: Nomu -> Categoria
categoria nomu 
    | fuerza nomu > 10000 = HighEnd
    | fuerza nomu > 3000  = Fuerte
    | fuerza nomu > 1000  = Comun
    | otherwise           = Pichi

-- "Para no cometer errores ortograficos que nos puede producir un string (ambiguedad)"
data Categoria = HighEnd | Fuerte | Comun | Pichi deriving(Show)  

-- PARTE 2
-- Otra cosa que no contemplamos es que los Nomus pueden
-- tener muchos poderes, como super regeneración, super fuerza, 
-- fuego, y teletransportación, entre otros… 

data Poder = UnPoder {
    cantCuracion :: Number,
    cantDanio :: Number,
    rango :: Number,
    probCritico :: Number
}deriving(Show)

-- Instanciamos algun poder..

superRegeneracion :: Poder
superRegeneracion = UnPoder 1000 0 5 0

superFuerza :: Poder
superFuerza = UnPoder 0 1000 5 10

-- 1) Averiguar la probabilidad de daño crítico del último poder que un Nomu consiguió.
-- 1ero. Me meto en los poderes del nomu
-- 2dos. Agarro el ultimo poder de la lista
-- 3ero. Agarro la probCritico de ese poder

probabilidadCriticoUltimoPoder :: Nomu -> Number
probabilidadCriticoUltimoPoder nomu = probCritico (last (poderes nomu))

-- 2) Saber si un poder es usado cuerpo a cuerpo, esto está definido por
-- su rango de ataque, siendo cuerpo a cuerpo si el rango es menor a 100.

-- "Casi siempre cuando preguntan de saber algo es un bool como respuesta"

esCuerpoACuerpo :: Poder -> Bool
esCuerpoACuerpo poder = rango poder < 100

-- 3) Saber si un poder es solamente de curación (esto sucede cuando no tiene
-- cantidad de daño por uso y si tiene cantidad de curacion por uso).

esSolamenteDeCuracion :: Poder -> Bool
esSolamenteDeCuracion poder = cantDanio poder == 0 && cantCuracion poder > 0

-- Instanciamos algunos nomus

-- Sin hacer falta respetar un orden (mas expresiva)
arturo :: Nomu
arturo = UnNomu {
    tieneAlas = True,
    tieneMultiplesBrazos = False,
    cantidadOjos = 0,
    colorDePiel = Blanco,
    cantVida = 900,
    fuerza = 5000,
    poderes = [superRegeneracion]
}

-- Nos obliga a respetar el orden del data (mas complicada, menos expresiva)
mateo :: Nomu
mateo = UnNomu True True 3 Gris 200 1000 [superRegeneracion, superFuerza]