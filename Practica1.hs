{-
	Nombre del módulo: Practica1
	Información de la versión: 2.0 (Practica1)
	Autor: Oscar Leonardo Olvera Ruiz.
	Profesor: Ulises Rodríguez Domínguez.
	Ayudante: Amir Gilberto Hidalgo Carrillo.
	Ayudante de laboratorio: Rodrigo Alejandro Sánchez Morales.
	Fecha: Martes 29 de septiembre del 2026.
	Copyright: © 2026 Oscar Leonardo Olvera Ruiz. Uso académico, Facultad de Ciencias, UNAM.
-}


{-
  ==================
  ¿Cómo ejecutarlo? 
  ==================
    runghc Practica1.hs            usa K = 10 y N = 30.
    runghc Practica1.hs 5 100      usa K = 5 filas por color y N = 100 columnas.
 
  O bien, se compila:
  
    ghc Practica1.hs -o practica1
    ./practica1
 
  El programa imprime la demostración de los tres ejercicios y escribe el archivo "imagen.txt" (ejercicio 3) en el directorio actual.
  Sólo se usa la biblioteca estándar (base).
-}


-- Defino el módulo principal para que el programa se pueda compilar como un ejecutable.
module Main where

-- Será para las funciones para listas, del modo: group (para eliminar repetidos), intercalate (para el formato de comas/flechas) y sortOn (para ordenar palabras).
import Data.List (group, intercalate, sortOn)

-- La importo para capturar los parámetros de entrada (K y N de la imagen) directamente desde la terminal.
import System.Environment (getArgs)

-- Se usa para forzar la salida de la consola a UTF-8 y evitar que los caracteres especiales (∅, ✓, ✗, ⇒) se rompan al imprimir.
import System.IO (hSetEncoding, stdout, utf8)

-- ==========
--  Contexto.
-- ==========

-- Símbolo que es un terminal (un carácter) o un no terminal (un nombre). El carácter '∅' para la cadena vacía.
data Simbolo = T Char | V String
  deriving (Eq, Show)

-- Forma sentencial (secuencia de símbolos).
type Forma = [Simbolo]

-- Una regla (producción) A ::= α.
data Regla = Regla { izq :: String, der :: Forma }
  deriving (Eq, Show)

-- Una gramática G = (Σ, V, S, P)
data Gramatica = Gramatica
  { sigma     :: String      -- Σ : terminales.
  , variables :: [String]    -- V : no terminales.
  , inicial   :: String      -- S : símbolo inicial.
  , reglas    :: [Regla]     -- P : producciones.
  }

-- Símbolo que denota la cadena vacía.
vacio :: Char
vacio = '∅'

epsilon :: Simbolo
epsilon = T vacio

-- Notación para escribir reglas: "S" ==> [T 'c', V "S", T 'c'].
infixr 1 ==>
(==>) :: String -> Forma -> Regla
a ==> alpha = Regla a alpha

-- Lista de terminales a partir de una cadena: ts "#0" = [T '#', T '0'].
ts :: String -> Forma
ts = map T

esVariable :: Simbolo -> Bool
esVariable (V _) = True
esVariable _     = False

-- La palabra que representa una forma, sus terminales, sin contar ∅.
palabra :: Forma -> String
palabra f = [c | T c <- f, c /= vacio]

numTerminales :: Forma -> Int
numTerminales = length . palabra

-- Aplica la regla r al no terminal más a la izquierda de la forma f.
aplicar :: Regla -> Forma -> Either String Forma
aplicar r f =
  case break esVariable f of
    (pref, V a : suf)
      | a == izq r -> Right (pref ++ der r ++ suf)
      | otherwise  -> Left ("la regla  " ++ mostrarRegla r ++ "  no se aplica a " ++ a)
    _              -> Left "la forma sentencial ya no tiene no terminales"

-- Derivación a la izquierda. 
--Se da la lista de reglas que se aplican (siempre sobre el no terminal más a la izquierda) y se obtiene la sucesión de formas sentenciales que recorre la derivación, empezando en el símbolo inicial.
derivacion :: Gramatica -> [Regla] -> Either String [Forma]
derivacion g = paso [V (inicial g)]
  where
    paso f []       = Right [f]
    paso f (r : rs)
      | r `notElem` reglas g =
          Left ("la regla  " ++ mostrarRegla r ++ "  no pertenece a la gramática")
      | otherwise = do
          f'    <- aplicar r f
          resto <- paso f' rs
          return (f : resto)

-- La palabra producida por una derivación completa.
producir :: Gramatica -> [Regla] -> Either String String
producir g rs = do
  formas <- derivacion g rs
  let ultima = last formas
  if any esVariable ultima
    then Left ("la derivación termina en  " ++ mostrarForma ultima ++ "  (aún quedan no terminales)")
    else Right (palabra ultima)

-- Ordena por longitud y luego alfabéticamente.
ordena :: [String] -> [String]
ordena = sortOn (\w -> (length w, w))

-- Quita repetidos (la lista debe venir ordenada).
sinRepetidos :: [String] -> [String]
sinRepetidos xs = [w | (w : _) <- group xs]

-- Todas las palabras que se derivan del no terminal 'a' y tienen a lo más n terminales (sin contar ∅). 
-- Se exploran todas las derivaciones a la izquierda; la poda por número de terminales termina siempre que cada regla recursiva agregue al menos un terminal (cierto en las gramáticas de esta práctica).
lenguajeDesde :: Gramatica -> String -> Int -> [String]
lenguajeDesde g a n = sinRepetidos (ordena (explora [[V a]]))
  where
    explora :: [Forma] -> [String]
    explora [] = []
    explora (f : fs)
      | numTerminales f > n = explora fs
      | otherwise =
          case break esVariable f of
            (pref, V b : suf) ->
              explora ([pref ++ der r ++ suf | r <- reglas g, izq r == b] ++ fs)
            _ -> palabra f : explora fs

-- L(G) restringido a palabras de longitud ≤ n.
lenguaje :: Gramatica -> Int -> [String]
lenguaje g = lenguajeDesde g (inicial g)

-- --------------------------------------------------------
--  Presentación en pantalla y utilidades para las pruebas.
-- --------------------------------------------------------

-- Un no terminal de varias letras se escribe entre < > para distinguirlo.
mostrarSimbolo :: Simbolo -> String
mostrarSimbolo (T c) = [c]
mostrarSimbolo (V n)
  | length n == 1 = n
  | otherwise     = "<" ++ n ++ ">"

mostrarForma :: Forma -> String
mostrarForma = concatMap mostrarSimbolo

mostrarPalabra :: String -> String
mostrarPalabra "" = "∅"
mostrarPalabra w  = w

-- Secuencia de símbolos separados por espacios.
simbolos :: Forma -> String
simbolos = unwords . map plano
  where
    plano (T c) = [c]
    plano (V n) = n

mostrarRegla :: Regla -> String
mostrarRegla (Regla a alpha) = a ++ " ::= " ++ simbolos alpha

-- Una entrada por no terminal, con todas sus alternativas.
entradas :: Gramatica -> [(String, [Forma])]
entradas g = [(a, [der r | r <- reglas g, izq r == a]) | a <- variables g]

conjunto :: [String] -> String
conjunto xs = "{" ++ intercalate ", " xs ++ "}"

-- Imprime G = (Σ, V, S, P) con P como lista numerada (una entrada por no terminal).
muestraGramatica :: String -> Gramatica -> IO ()
muestraGramatica nombre g = do
  putStrLn (nombre ++ " = (Σ, V, S, P)")
  putStrLn ("  Σ = " ++ conjunto (map (: []) (sigma g)))
  putStrLn ("  V = " ++ conjunto (variables g))
  putStrLn ("  S = " ++ inicial g)
  putStrLn "  P:"
  mapM_ imprime (zip [1 ..] (entradas g))
  where
    imprime (i, (a, alts)) =
      putStrLn ("    " ++ show i ++ ". " ++ a ++ " ::= " ++ intercalate " | " (map simbolos alts))

rellena :: Int -> String -> String
rellena w s = s ++ replicate (w - length s) ' '

titulo :: String -> IO ()
titulo s = do
  putStrLn ""
  putStrLn (replicate 72 '=')
  putStrLn s
  putStrLn (replicate 72 '=')

siNo :: Bool -> String
siNo True  = "sí"
siNo False = "no"

verifica :: String -> Bool -> IO ()
verifica texto ok = putStrLn ("  " ++ (if ok then "✓ " else "✗ ") ++ texto)

-- Muestra una derivación paso a paso, indicando la regla usada en cada paso.
demuestra :: Gramatica -> [Regla] -> IO ()
demuestra g rs =
  case derivacion g rs of
    Left err -> putStrLn ("  ✗ derivación inválida: " ++ err)
    Right formas -> do
      let textos = map mostrarForma formas
          ancho  = maximum (map length textos)
      putStrLn ("  " ++ head textos)
      mapM_ (\(txt, r) -> putStrLn ("    ⇒ " ++ rellena ancho txt ++ "   [" ++ mostrarRegla r ++ "]"))
            (zip (tail textos) rs)
      putStrLn ("  palabra producida: " ++ mostrarPalabra (palabra (last formas)))

-- Todas las cadenas de longitud n sobre un alfabeto (para verificar por fuerza bruta).
palabrasDeLongitud :: String -> Int -> [String]
palabrasDeLongitud alfabeto n = sequence (replicate n alfabeto)

-- =================
--  1.- Palíndromos.
-- =================
--
--  G1 = (Σ, V, S, P)
--  Σ = {c, u, ∅}          (∅ denota la cadena vacía).
--  V = {S}
--  S = S
--  P:
--    S ::= c S c | u S u | c | u | ∅
--
--  Ya que un palíndromo se construye de afuera hacia adentro; se pone la misma letra a ambos lados (c S c, u S u) y se termina con el centro: vacío (palíndromos de longitud par) o una sola letra (longitud impar).
--
--  Corrección: L(G1) = { w ∈ {c,u}* : w = w^R }.
--   (⊆) Inducción sobre el número de pasos: ∅, c, u son palíndromos y, si w lo es, también c w c y u w u.
--   (⊇) Inducción fuerte sobre |w|: si |w| ≤ 1, w ∈ {∅, c, u} se obtiene con una sola regla; si |w| ≥ 2, w = a w' a con w' palíndromo más corto, y S ⇒ a S a ⇒* a w' a.

p1CSC, p1USU, p1C, p1U, p1Vacia :: Regla
p1CSC   = "S" ==> [T 'c', V "S", T 'c']
p1USU   = "S" ==> [T 'u', V "S", T 'u']
p1C     = "S" ==> [T 'c']
p1U     = "S" ==> [T 'u']
p1Vacia = "S" ==> [epsilon]

gPalindromos :: Gramatica
gPalindromos = Gramatica
  { sigma     = "cu∅"
  , variables = ["S"]
  , inicial   = "S"
  , reglas    = [p1CSC, p1USU, p1C, p1U, p1Vacia]
  }

-- La derivación de un palíndromo w (si w lo es), se van quitando las letras extremas iguales hasta llegar al centro.
derivaPalindromo :: String -> Maybe [Regla]
derivaPalindromo w =
  case w of
    ""  -> Just [p1Vacia]
    "c" -> Just [p1C]
    "u" -> Just [p1U]
    (x : resto)
      | not (null resto) && x == last resto && x == 'c' ->
          fmap (p1CSC :) (derivaPalindromo (init resto))
      | not (null resto) && x == last resto && x == 'u' ->
          fmap (p1USU :) (derivaPalindromo (init resto))
    _ -> Nothing

esPalindromo :: String -> Bool
esPalindromo w = w == reverse w

ejemplo1 :: String -> IO ()
ejemplo1 w = do
  putStrLn ("• " ++ mostrarPalabra w)
  case derivaPalindromo w of
    Just rs -> demuestra gPalindromos rs
    Nothing -> putStrLn "  (no es un palíndromo sobre {c, u})"

ejercicio1 :: IO ()
ejercicio1 = do
  titulo "EJERCICIO 1 — Palíndromos"
  muestraGramatica "G1" gPalindromos
  putStrLn "  (∅ denota la cadena vacía)"
  putStrLn ""
  putStrLn "Ejemplos de producción (derivaciones a la izquierda):"
  mapM_ ejemplo1 ["", "uu", "cuuc", "uccu", "ccuucc", "cuc"]
  putStrLn ""
  putStrLn "Verificación automática (palabras de longitud ≤ 8):"
  let hasta     = 8
      generadas = lenguaje gPalindromos hasta
      esperadas = ordena [ w | n <- [0 .. hasta]
                             , w <- palabrasDeLongitud "cu" n
                             , esPalindromo w ]
  putStrLn ("  palabras que genera G1 .................. " ++ show (length generadas))
  putStrLn ("  palíndromos de {c,u}* (fuerza bruta) .... " ++ show (length esperadas))
  verifica "G1 genera exactamente los palíndromos (hasta longitud 8)" (generadas == esperadas)

-- ==========================
--  2.- Movimientos cíclicos.
-- ==========================
--
--  Movimientos sobre la rejilla (columna, fila):
--
--        d=(-1, 1)   U=( 0, 1)   a=( 1, 1)
--        L=(-1, 0)       •       R=( 1, 0)
--        c=(-1,-1)   D=( 0,-1)   b=( 1,-1)
--
--  Opuestos:  U↔D   L↔R   a↔c   b↔d          (el desplazamiento del opuesto es el negativo del desplazamiento del movimiento).
--
--  G2 = (Σ, V, S, P)
--  Σ = {U, D, L, R, a, b, c, d}
--  V = {S}
--  S = S
--  P:
--    S ::= U S D | D S U | L S R | R S L | a S c | c S a | b S d | d S b | ∅
--
--  (∅ para la cadena vacía, como en el ejercicio 1.)
--
--  Teorema 1 (regresa al origen). Si δ(w) es la suma de los desplazamientos de los movimientos de w, entonces δ(w) = (0,0) para toda w ∈ L(G2). Inducción sobre la derivación: δ(∅) = (0,0); si δ(w) = (0,0), entonces δ(x w x̄) = δ(x) + δ(w) − δ(x) = (0,0).
--
--  Teorema 2 (estructura). L(G2) = { x1 x2 … xn x̄n … x̄2 x̄1 : n ≥ 0, xi ∈ Σ }, donde x̄ es el opuesto de x: cada regla agrega un movimiento al principio y su opuesto al final, de modo que cada movimiento queda emparejado con su opuesto (en LUdUUDDbDR:  L–R, U–D, d–b, U–D, U–D).
--
--  Notita: para regresar al origen es necesario pero no suficiente para estar en L(G2) (o sea... ULDR regresa al origen y no se genera). Con base en las instrucciones entiendo que se pide lo contrario (que todo lo que genera G2 regrese), y eso da con el Teorema 1.

mov :: Char -> (Int, Int)
mov 'U' = ( 0,  1)
mov 'D' = ( 0, -1)
mov 'L' = (-1,  0)
mov 'R' = ( 1,  0)
mov 'a' = ( 1,  1)
mov 'b' = ( 1, -1)
mov 'c' = (-1, -1)
mov 'd' = (-1,  1)
mov _   = ( 0,  0)

paresOpuestos :: [(Char, Char)]
paresOpuestos =
  [('U', 'D'), ('D', 'U'), ('L', 'R'), ('R', 'L'), ('a', 'c'), ('c', 'a'), ('b', 'd'), ('d', 'b')]

opuesto :: Char -> Char
opuesto x = maybe x id (lookup x paresOpuestos)

-- Regla  S ::= x S y   para un par de movimientos opuestos (x, y).
reglaPar :: (Char, Char) -> Regla
reglaPar (x, y) = "S" ==> [T x, V "S", T y]

p2Vacia :: Regla
p2Vacia = "S" ==> [epsilon]

gMovimientos :: Gramatica
gMovimientos = Gramatica
  { sigma     = "UDLRabcd"
  , variables = ["S"]
  , inicial   = "S"
  , reglas    = map reglaPar paresOpuestos ++ [p2Vacia]
  }

suma :: (Int, Int) -> (Int, Int) -> (Int, Int)
suma (a, b) (c, d) = (a + c, b + d)

-- δ(w), desplazamiento total de una secuencia de movimientos.
desplazamiento :: String -> (Int, Int)
desplazamiento = foldl suma (0, 0) . map mov

-- Posiciones por las que pasa el punto, empezando en (0,0).
trayectoria :: String -> [(Int, Int)]
trayectoria = scanl suma (0, 0) . map mov

regresaAlOrigen :: String -> Bool
regresaAlOrigen w = desplazamiento w == (0, 0)

-- w ∈ L(G2)  ⇔  w = x1 … xn x̄n … x̄1  (Teorema 2).
perteneceG2 :: String -> Bool
perteneceG2 w =
  all (`elem` sigma gMovimientos) w
    && even (length w)
    && drop m w == map opuesto (reverse (take m w))
  where
    m = length w `div` 2

-- La derivación de una palabra de L(G2). Una regla S ::= x S x̄ por cada movimiento x de la primera mitad, y al final S ::= ∅.
derivaMov :: String -> Maybe [Regla]
derivaMov w
  | perteneceG2 w = Just ([reglaPar (x, opuesto x) | x <- mitad] ++ [p2Vacia])
  | otherwise     = Nothing
  where
    mitad = take (length w `div` 2) w

ejemploMov :: String -> IO ()
ejemploMov w = do
  putStrLn ("• " ++ w)
  case derivaMov w of
    Just rs -> demuestra gMovimientos rs
    Nothing -> putStrLn "  (no pertenece a L(G2))"
  putStrLn ("  trayectoria: " ++ intercalate " → " (map show (trayectoria w)))

filaTabla :: String -> IO ()
filaTabla w =
  putStrLn ("  " ++ rellena 14 w
                  ++ rellena 12 (siNo (perteneceG2 w))
                  ++ rellena 17 (show (desplazamiento w))
                  ++ siNo (regresaAlOrigen w))

ejercicio2 :: IO ()
ejercicio2 = do
  titulo "EJERCICIO 2 — Movimientos cíclicos"
  muestraGramatica "G2" gMovimientos
  putStrLn "  (∅ denota la cadena vacía, igual que en el ejercicio 1)"
  putStrLn ""
  putStrLn "Movimientos y su desplazamiento (columna, fila):"
  putStrLn "  d=(-1,1)   U=(0,1)    a=(1,1)"
  putStrLn "  L=(-1,0)   •          R=(1,0)"
  putStrLn "  c=(-1,-1)  D=(0,-1)   b=(1,-1)"
  putStrLn "  opuestos:  U↔D   L↔R   a↔c   b↔d"
  putStrLn ""
  putStrLn "Ejemplos válidos (derivación y trayectoria desde (0,0)):"
  mapM_ ejemploMov ["UD", "db", "LUdUUDDbDR"]
  putStrLn ""
  putStrLn "Tabla de ejemplos válidos y no válidos:"
  putStrLn "  palabra       ¿en L(G2)?  desplazamiento   ¿regresa al origen?"
  mapM_ filaTabla ["UD", "db", "LUdUUDDbDR", "UDd", "dd", "LUdUUDDbD", "U"]
  putStrLn ""
  putStrLn "Verificación automática (palabras de longitud ≤ 8):"
  let hasta     = 8
      generadas = lenguaje gMovimientos hasta
      esperadas = ordena [ h ++ map opuesto (reverse h)
                         | m <- [0 .. hasta `div` 2]
                         , h <- palabrasDeLongitud (sigma gMovimientos) m ]
  putStrLn ("  palabras que genera G2 .................. " ++ show (length generadas))
  verifica "L(G2) = { x1…xn · op(xn)…op(x1) }  (cada movimiento con su opuesto, en espejo)"
           (generadas == esperadas)
  verifica "toda palabra generada regresa al punto de partida" (all regresaAlOrigen generadas)
  verifica "ULDR regresa al origen pero no se genera (necesario, no suficiente)"
           (regresaAlOrigen "ULDR" && not (perteneceG2 "ULDR"))

-- ============================
--  3.- Generación de imágenes.
-- ============================
--
--  Se parte de la gramática general de colores  S ::= # X X X X X X  y se fijan en 0 los dos canales que no corresponden, de modo que sólo se produzcan intensidades de rojo (#XX0000), de verde (#00XX00) y de azul (#0000XX), con X ∈ {0,…,9,A,…,F}.
--
--  G3 = (Σ, V, S, P)
--  Σ = {#, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F}
--  V = {S, Rojo, Verde, Azul, X}
--  S = S
--  P:
--    S ::= Rojo | Verde | Azul
--    Rojo ::= # X X 0 0 0 0
--    Verde ::= # 0 0 X X 0 0
--    Azul ::= # 0 0 0 0 X X
--    X ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | A | B | C | D | E | F
--
--  L(G3) tiene 3·256 − 2 = 766 palabras distintas: la palabra #000000 se deriva de las tres bandas (G3 es ambigua sólo en esa palabra).
--
--  Imagen de 3K × N píxeles: las K primeras filas son rojas, las K siguientes verdes y las K últimas azules. En la columna j (0 ≤ j ≤ N−1) la intensidad es  ⌊255·j / (N−1)⌋,  así que cada banda es un degradado de 00 a FF. Cada píxel se obtiene con una derivación de  G3:  S ::= banda,  banda ::= # … X X …,  X ::= d1,  X ::= d2.
--
--  El formato a seguir sera: primera línea "K,N"; después 3K líneas con N colores separados por comas (cada línea termina en coma).

digitosHex :: String
digitosHex = "0123456789ABCDEF"

p3SRojo, p3SVerde, p3SAzul, p3Rojo, p3Verde, p3Azul :: Regla
p3SRojo  = "S"     ==> [V "Rojo"]
p3SVerde = "S"     ==> [V "Verde"]
p3SAzul  = "S"     ==> [V "Azul"]
p3Rojo   = "Rojo"  ==> ts "#"     ++ [V "X", V "X"] ++ ts "0000"
p3Verde  = "Verde" ==> ts "#00"   ++ [V "X", V "X"] ++ ts "00"
p3Azul   = "Azul"  ==> ts "#0000" ++ [V "X", V "X"]

p3Digito :: Char -> Regla
p3Digito d = "X" ==> [T d]

gColores :: Gramatica
gColores = Gramatica
  { sigma     = '#' : digitosHex
  , variables = ["S", "Rojo", "Verde", "Azul", "X"]
  , inicial   = "S"
  , reglas    = [p3SRojo, p3SVerde, p3SAzul, p3Rojo, p3Verde, p3Azul] ++ map p3Digito digitosHex
  }

data Banda = BRojo | BVerde | BAzul
  deriving (Eq, Show)

bandas :: [Banda]
bandas = [BRojo, BVerde, BAzul]

nombreBanda :: Banda -> String
nombreBanda BRojo  = "rojo"
nombreBanda BVerde = "verde"
nombreBanda BAzul  = "azul"

-- La derivación del color de la banda b con intensidad v (0 ≤ v ≤ 255): las dos reglas de la banda y las dos reglas  X ::= dígito (nibble alto y bajo).
derivacionColor :: Banda -> Int -> [Regla]
derivacionColor b v = [rS, rColor, p3Digito (hex (v `div` 16)), p3Digito (hex (v `mod` 16))]
  where
    (rS, rColor) = case b of
      BRojo  -> (p3SRojo,  p3Rojo)
      BVerde -> (p3SVerde, p3Verde)
      BAzul  -> (p3SAzul,  p3Azul)
    hex i = digitosHex !! i

-- El color (cadena "#RRGGBB") producido por la derivación.
color :: Banda -> Int -> String
color b v =
  case producir gColores (derivacionColor b v) of
    Right w  -> w
    Left err -> error err

-- Intensidad en la columna j de una imagen con n columnas (n ≥ 2).
intensidad :: Int -> Int -> Int
intensidad n j = (255 * j) `div` (n - 1)

-- Un renglón de n píxeles de la banda b.
renglon :: Banda -> Int -> [String]
renglon b n = [color b (intensidad n j) | j <- [0 .. n - 1]]

-- La imagen, 3K renglones de N colores (K rojos, K verdes, K azules).
imagen :: Int -> Int -> [[String]]
imagen k n = concat [replicate k (renglon b n) | b <- bandas]

-- Se escribe el archivo de texto, primera línea "K,N" y luego los 3K renglones.
escribeImagen :: FilePath -> Int -> Int -> IO ()
escribeImagen ruta k n =
  writeFile ruta (unlines (cabecera : map linea (imagen k n)))
  where
    cabecera = show k ++ "," ++ show n
    linea cs = intercalate "," cs ++ ","

-- Parte una cadena en los trozos separados por el carácter d.
divide :: Char -> String -> [String]
divide d s =
  case break (== d) s of
    (a, [])        -> [a]
    (a, _ : resto) -> a : divide d resto

-- ¿Todos los colores de las filas son palabras del lenguaje que se deriva de a?
enLenguaje :: String -> [[String]] -> Bool
enLenguaje a filas = all (`elem` lang) distintos
  where
    lang      = lenguajeDesde gColores a 7
    distintos = sinRepetidos (ordena (concat filas))

muestra :: String -> [[String]] -> IO ()
muestra nombre filas =
  case filas of
    (r : _) -> putStrLn ("  " ++ nombre ++ ": " ++ intercalate "," (take 6 r) ++ ",…")
    []      -> return ()

-- Vuelve a leer el archivo escrito y comprueba su estructura y que cada píxel sea una palabra de L(G3) de la banda que le corresponde.
verificaImagen :: FilePath -> IO ()
verificaImagen ruta = do
  contenido <- readFile ruta
  case lines contenido of
    [] -> putStrLn "  ✗ el archivo está vacío"
    (primera : resto) -> do
      let [k, n]           = map read (divide ',' primera) :: [Int]
          renglones        = map (filter (not . null) . divide ',') resto
          (rojas, r1)      = splitAt k renglones
          (verdes, azules) = splitAt k r1
      putStrLn ("  primera línea del archivo: " ++ primera)
      verifica ("hay 3K = " ++ show (3 * k) ++ " renglones") (length renglones == 3 * k)
      verifica ("cada renglón tiene N = " ++ show n ++ " colores") (all ((== n) . length) renglones)
      verifica "las K primeras filas son rojas (cada píxel ∈ L(G3) derivado desde Rojo)"
               (enLenguaje "Rojo" rojas)
      verifica "las K filas siguientes son verdes" (enLenguaje "Verde" verdes)
      verifica "las K últimas filas son azules" (enLenguaje "Azul" azules)
      muestra "fila roja " rojas
      muestra "fila verde" verdes
      muestra "fila azul " azules

ejemploColor :: Banda -> Int -> IO ()
ejemploColor b v = do
  putStrLn ("• " ++ nombreBanda b ++ ", intensidad " ++ show v
            ++ " (" ++ [digitosHex !! (v `div` 16), digitosHex !! (v `mod` 16)] ++ " en hexadecimal)")
  demuestra gColores (derivacionColor b v)

ejercicio3 :: Int -> Int -> IO ()
ejercicio3 k n = do
  titulo "EJERCICIO 3 — Generación de imágenes"
  muestraGramatica "G3" gColores
  putStrLn ""
  putStrLn "Ejemplos de producción de colores (uno por banda):"
  ejemploColor BRojo 0xAB
  ejemploColor BVerde 0x7F
  ejemploColor BAzul 0xC8
  putStrLn ""
  let lang = lenguaje gColores 7
  verifica ("L(G3) tiene " ++ show (length lang)
            ++ " palabras distintas = 3·256 − 2 (#000000 se deriva en las 3 bandas)")
           (length lang == 3 * 256 - 2)
  putStrLn ""
  putStrLn ("Imagen de 3K × N = " ++ show (3 * k) ++ " × " ++ show n
            ++ "   (K = " ++ show k ++ ", N = " ++ show n ++ ")")
  escribeImagen "imagen.txt" k n
  putStrLn "Archivo escrito: imagen.txt   (se vuelve a leer y se verifica):"
  verificaImagen "imagen.txt"
  putStrLn ""

-- ========================
--  Programa listo k listo.
-- ========================

-- K y N pueden darse como argumentos (K ≥ 5 y N ≥ 5); por omisión K = 10, N = 30.
parametros :: [String] -> (Int, Int)
parametros [a, b]
  | [(k, "")] <- reads a, [(n, "")] <- reads b, k >= 5, n >= 5 = (k, n)
parametros _ = (10, 30)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  let (k, n) = parametros args
  ejercicio1
  ejercicio2
  ejercicio3 k n
