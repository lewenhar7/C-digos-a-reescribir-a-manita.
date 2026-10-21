{-
	Nombre del módulo: Practica2
	Información de la versión: 3.0 (Practica2)
	Autor: Oscar Leonardo Olvera Ruiz.
	Profesor: Ulises Rodríguez Domínguez.
	Ayudante: Amir Gilberto Hidalgo Carrillo.
	Ayudante de laboratorio: Rodrigo Alejandro Sánchez Morales.
	Fecha: Jueves 08 de octubre del 2026.
	Copyright: © 2026 Oscar Leonardo Olvera Ruiz. Uso académico, Facultad de Ciencias, UNAM.
-}

{-
  ==================
  ¿Cómo ejecutarlo? 
  ==================
    runghc Practica2.hs                  es la demostración de los tres ejercicios.
    runghc Practica2.hs mi_imagen.csv    aplica la apertura del ejercicio 3 a la propia imagen (0/1 separados por comas, un renglón por línea).
 
  O bien, se compila:
  
    ghc Practica2.hs -o practica2
    ./practica2
 
  Todo se simula con variables Bool. 
  Los circuitos se construyen sólo con las compuertas AND, OR y NOT.  
  Se usa únicamente la biblioteca estándar.
  El ejercicio 3 escribirá el archivo "apertura.txt" en el directorio actual.
-}

-- Defino el módulo principal para que el programa se pueda compilar como un ejecutable.
module Main where

-- Será para las funciones para listas, como por ejemplo, se usará "intercalate" que nos servirá en el 3, para dar formato al archivo de texto separado por comas.
import Data.List (foldl', intercalate, transpose)

-- La importo para capturar los parámetros de entrada (distancias, votos o pesos de las rutas) directamente desde la terminal.
import System.Environment (getArgs)

-- Se usa para forzar la salida de la consola a UTF-8 y evitar que los caracteres especiales (⊕, ✓, ✗, ⇒) se rompan al imprimir.
import System.IO (hSetEncoding, stdout, utf8)

-- ==========
--  Contexto.
-- ==========

-- Un bit es el valor lógico donde False = 0, True = 1.
type Bit = Bool

-- Compuertas base.
notG :: Bit -> Bit
notG = not

andG :: Bit -> Bit -> Bit
andG a b = a && b

orG :: Bit -> Bit -> Bit
orG a b = a || b

-- Compuertas construidas con AND, OR y NOT a XOR b = (a ∨ b) ∧ ¬(a ∧ b).
xorG :: Bit -> Bit -> Bit
xorG a b = andG (orG a b) (notG (andG a b))

xnorG :: Bit -> Bit -> Bit
xnorG a b = notG (xorG a b)

-- Multiplexor de 2 a 1. Si s = 1 sale a, si s = 0 sale b.  y = (s ∧ a) ∨ (¬s ∧ b).
muxG :: Bit -> Bit -> Bit -> Bit
muxG s a b = orG (andG s a) (andG (notG s) b)

-- Un binario es una lista de bits con el bit MENOS significativo primero.
type Binario = [Bit]

-- n bits del entero v ≥ 0 (lo usare para meter datos al circuito y para mostrarlos).
aBits :: Int -> Int -> Binario
aBits n v = [odd (v `div` (2 ^ i)) | i <- [0 .. n - 1]]

-- El entero que representa una lista de bits.
deBits :: Binario -> Int
deBits = foldr (\b acc -> (if b then 1 else 0) + 2 * acc) 0

bitChar :: Bit -> Char
bitChar b = if b then '1' else '0'

-- Texto con el bit más significativo primero. 5 (4 bits) → "0101".
binario :: Binario -> String
binario = map bitChar . reverse

-- Texto de los bits en el orden de la lista.
enOrden :: Binario -> String
enOrden = map bitChar

-- Completa con ceros (bits más significativos) hasta tener n bits.
extiende :: Int -> Binario -> Binario
extiende n bs = bs ++ replicate (n - length bs) False

-- Semisumador. suma = a ⊕ b, acarreo = a ∧ b.
semiSumador :: Bit -> Bit -> (Bit, Bit)
semiSumador a b = (xorG a b, andG a b)

-- Sumador completo (dos semisumadores y un OR). suma = a ⊕ b ⊕ cin, cout = (a ∧ b) ∨ (cin ∧ (a ⊕ b)).
sumadorCompleto :: Bit -> Bit -> Bit -> (Bit, Bit)
sumadorCompleto a b cin = (s2, orG c1 c2)
  where
    (s1, c1) = semiSumador a b
    (s2, c2) = semiSumador s1 cin

-- Sumador binario de n bits (o sea... un acarreo propagado de un bit al siguiente).
-- Recibe el acarreo de entrada y dos números del mismo ancho; devuelve la suma (del mismo ancho) y el acarreo de salida.
sumadorN :: Bit -> Binario -> Binario -> (Binario, Bit)
sumadorN cin [] _ = ([], cin)
sumadorN cin _ [] = ([], cin)
sumadorN cin (x : xs) (y : ys) = (s : ss, cout)
  where
    (s, c1)    = sumadorCompleto x y cin
    (ss, cout) = sumadorN c1 xs ys

-- Suma de dos números de n bits con resultado de n+1 bits.
sumador :: Binario -> Binario -> Binario
sumador xs ys = s ++ [c]
  where
    (s, c) = sumadorN False xs ys

-- Comparador de n bits en cascada.  
-- Una celda por bit, del menos al más significativo; el bit nuevo manda sobre lo decidido por los bits menos significativos.  
-- Con e = a XNOR b:
--     LT' = (¬a ∧ b) ∨ (e ∧ LT)
--     EQ' =  e ∧ EQ
--     GT' = (a ∧ ¬b) ∨ (e ∧ GT)     empezando en (LT, EQ, GT) = (0, 1, 0).
-- Devuelve (a < b, a = b, a > b).
comparador :: Binario -> Binario -> (Bit, Bit, Bit)
comparador xs ys = foldl celda (False, True, False) (zip xs ys)
  where
    celda (lt, eq, gt) (a, b) =
      ( orG (andG (notG a) b) (andG e lt)
      , andG e eq
      , orG (andG a (notG b)) (andG e gt)
      )
      where
        e = xnorG a b

menorQue :: Binario -> Binario -> Bit
menorQue xs ys = lt
  where
    (lt, _, _) = comparador xs ys

igualA :: Binario -> Binario -> Bit
igualA xs ys = eq
  where
    (_, eq, _) = comparador xs ys

mayorQue :: Binario -> Binario -> Bit
mayorQue xs ys = gt
  where
    (_, _, gt) = comparador xs ys

mayorOIgual :: Binario -> Binario -> Bit
mayorOIgual xs ys = notG (menorQue xs ys)

-- Multiplexor de n bits. s = 1 elige xs, s = 0 elige ys.
muxN :: Bit -> Binario -> Binario -> Binario
muxN s = zipWith (muxG s)

-- Máximo de dos números del mismo ancho. comparador + multiplexor.
maximoN :: Binario -> Binario -> Binario
maximoN xs ys = muxN (mayorOIgual xs ys) xs ys

-- --------------------------------------------------------
--  Presentación en pantalla y utilidades para las pruebas.
-- --------------------------------------------------------

titulo :: String -> IO ()
titulo s = do
  putStrLn ""
  putStrLn (replicate 76 '=')
  putStrLn s
  putStrLn (replicate 76 '=')

rellena :: Int -> String -> String
rellena w s = s ++ replicate (w - length s) ' '

verifica :: String -> Bool -> IO ()
verifica mensaje ok = putStrLn ("  " ++ (if ok then "✓ " else "✗ ") ++ mensaje)

-- Generador pseudoaleatorio (congruencial lineal) para las pruebas; según lo que entendí... así no hace falta ningún paquete fuera de base.
siguiente :: Int -> Int
siguiente s = (1103515245 * s + 12345) `mod` 2147483648

-- k enteros pseudoaleatorios en [lo, hi] a partir de una semilla.
aleatorios :: Int -> Int -> Int -> Int -> [Int]
aleatorios semilla k lo hi =
  take k [lo + (s `div` 65536) `mod` (hi - lo + 1) | s <- tail (iterate siguiente semilla)]

-- ========================
--  1.- Decidiendo caminos.
-- ========================
--
--  Entradas (en binario): d1, c1, d2, c2 de 6 bits (valores 1..32) y w1, w2 de 4 bits (valores 1..8).
--
--  Diseño del circuito:
--  
--    d1,d2 ──▶ [ COMPARADOR 6 bits ] ──▶ mejor en distancia.
--    c1,c2 ──▶ [ COMPARADOR 6 bits ] ──▶ mejor en costo.
--
--    w1 ──▶ [ TERMÓMETRO ] ──▶ t1 ─┐
--    w2 ──▶ [ TERMÓMETRO ] ──▶ t2 ─┤
--    (t1, d1) ─▶ [ MULT ] ─┐
--    (t2, c1) ─▶ [ MULT ] ─┴▶ [ SUMADOR 10 bits ] ─▶ m1 ─┐
--    (t1, d2) ─▶ [ MULT ] ─┐                              ├▶ [ COMPARADOR 10 bits ] ─▶ mejor compuesta.
--    (t2, c2) ─▶ [ MULT ] ─┴▶ [ SUMADOR 10 bits ] ─▶ m2 ─┘
--
--  * Respuestas simples. La salida "a > b" del comparador vale 1 cuando la ruta 1 es peor; se interpreta 0 → R1, 1 → R2. En un empate se elige R1.
--  * Multiplicación por sumas repetidas. El termómetro de w es [w ≥ 1, w ≥ 2, …, w ≥ 8] (se obtiene con el comparador contra constantes).
--    El multiplicador tiene 8 etapas; la etapa j suma al acumulador, con el sumador binario, una copia del operando enmascarada (AND) con la señal "w ≥ j".  Así el operando se acumula exactamente w veces, sin lazos que dependan de los datos: es un circuito combinacional.
--  * Anchos. w·d ≤ 8·32 = 256 y w1·d + w2·c ≤ 512 < 2^10, así que con acumuladores de 10 bits nunca hay desbordamiento.

data Ruta = R1 | R2
  deriving (Eq, Show)

-- La salida de un comparador "a > b" se interpreta: 0 → R1, 1 → R2.
bitARuta :: Bit -> Ruta
bitARuta False = R1
bitARuta True  = R2

anchoEntrada, anchoPeso, anchoMetrica :: Int
anchoEntrada = 6     -- d_i, c_i ∈ [1, 32].
anchoPeso    = 4     -- w_k ∈ [1, 8].
anchoMetrica = 10    -- w1·d + w2·c ≤ 512.

-- Decodificador "termómetro" de un peso w: [w ≥ 1, w ≥ 2, …, w ≥ 8].
termometro :: Binario -> Binario
termometro w = [mayorOIgual w (aBits (length w) j) | j <- [1 .. 8]]

-- Etapa del multiplicador dnde suma al acumulador una copia de x enmascarada con t.
etapaMult :: Binario -> Binario -> Bit -> Binario
etapaMult x acc t = fst (sumadorN False acc (map (andG t) x))

-- Multiplicador por sumas repetidas, x · w, con el termómetro de w ya calculado.
multiplica :: Binario -> Binario -> Binario
multiplica term x = foldl (etapaMult x) (replicate (length x) False) term

-- Métrica compuesta  w1·d + w2·c  (d y c ya extendidos a anchoMetrica bits); t1 y t2 son los termómetros de w1 y w2.
metrica :: Binario -> Binario -> Binario -> Binario -> Binario
metrica t1 t2 d c = fst (sumadorN False (multiplica t1 d) (multiplica t2 c))

data Resultado1 = Resultado1
  { rDistancia :: Ruta      -- Mejor ruta en distancia.
  , rCosto     :: Ruta      -- Mejor ruta en costo.
  , rCompuesta :: Ruta      -- Mejor ruta con la métrica w1·d + w2·c.
  , empDist    :: Bit       -- Empate en distancia.
  , empCosto   :: Bit       -- Empate en costo.
  , empComp    :: Bit       -- Empate en la métrica compuesta.
  , m1         :: Binario   -- w1·d1 + w2·c1.
  , m2         :: Binario   -- w1·d2 + w2·c2.
  }

-- El circuito completo (todas las entradas en binario, bit menos significativo primero).
rutas :: Binario -> Binario -> Binario -> Binario -> Binario -> Binario -> Resultado1
rutas d1 c1 d2 c2 w1 w2 = Resultado1
  { rDistancia = bitARuta (mayorQue d1 d2)
  , rCosto     = bitARuta (mayorQue c1 c2)
  , rCompuesta = bitARuta (mayorQue met1 met2)
  , empDist    = igualA d1 d2
  , empCosto   = igualA c1 c2
  , empComp    = igualA met1 met2
  , m1         = met1
  , m2         = met2
  }
  where
    ext  = extiende anchoMetrica
    t1   = termometro w1
    t2   = termometro w2
    met1 = metrica t1 t2 (ext d1) (ext c1)
    met2 = metrica t1 t2 (ext d2) (ext c2)

-- Supuestos del enunciado. 0 < w ≤ 8, 0 < d ≤ 32, 0 < c ≤ 32.
entradasValidas :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
entradasValidas d1 c1 d2 c2 w1 w2 =
  all (\v -> v >= 1 && v <= 32) [d1, c1, d2, c2]
    && all (\w -> w >= 1 && w <= 8) [w1, w2]

-- Atajo. Recibe enteros, los convierte a binario y usa el circuito.
recomendarRuta :: Int -> Int -> Int -> Int -> Int -> Int -> Resultado1
recomendarRuta d1 c1 d2 c2 w1 w2 =
  rutas (aBits anchoEntrada d1) (aBits anchoEntrada c1)
        (aBits anchoEntrada d2) (aBits anchoEntrada c2)
        (aBits anchoPeso w1)    (aBits anchoPeso w2)

veredicto :: Ruta -> Bit -> String
veredicto r emp
  | emp       = "R1  (empate: se elige R1)"
  | otherwise = show r

-- Muestra con detalle cómo el multiplicador acumula w copias de d.
detalleMultiplicador :: Int -> Int -> IO ()
detalleMultiplicador w d = do
  let term  = termometro (aBits anchoPeso w)
      x     = extiende anchoMetrica (aBits anchoEntrada d)
      pasos = scanl (etapaMult x) (replicate anchoMetrica False) term
  putStrLn ("• Multiplicador por sumas repetidas:  w = " ++ show w ++ ",  d = " ++ show d
            ++ "  (d en binario: " ++ binario x ++ ")")
  putStrLn ("    termómetro de w  [w≥1 … w≥8] = " ++ enOrden term)
  mapM_ (\(j, acc) -> putStrLn ("    después de la etapa " ++ show j ++ ":  "
                                ++ binario acc ++ "  (" ++ show (deBits acc) ++ ")"))
        (zip [1 ..] (tail pasos))
  putStrLn ("    resultado = " ++ show (deBits (last pasos)) ++ "   (w · d = " ++ show (w * d) ++ ")")

muestraCaso :: (Int, Int, Int, Int, Int, Int) -> IO ()
muestraCaso (d1, c1, d2, c2, w1, w2)
  | not (entradasValidas d1 c1 d2 c2 w1 w2) =
      putStrLn ("• (d1, c1, d2, c2, w1, w2) = " ++ show (d1, c1, d2, c2, w1, w2)
                ++ "  → fuera de los supuestos (0 < w ≤ 8, 0 < d ≤ 32, 0 < c ≤ 32)")
  | otherwise = do
      let r = recomendarRuta d1 c1 d2 c2 w1 w2
          bin n v = binario (aBits n v)
      putStrLn ("• R1: d1 = " ++ show d1 ++ ", c1 = " ++ show c1
                ++ "    R2: d2 = " ++ show d2 ++ ", c2 = " ++ show c2
                ++ "    pesos: w1 = " ++ show w1 ++ ", w2 = " ++ show w2)
      putStrLn ("    binario:  d1 = " ++ bin anchoEntrada d1 ++ "  c1 = " ++ bin anchoEntrada c1
                ++ "  d2 = " ++ bin anchoEntrada d2 ++ "  c2 = " ++ bin anchoEntrada c2
                ++ "  w1 = " ++ bin anchoPeso w1 ++ "  w2 = " ++ bin anchoPeso w2)
      putStrLn ("    mejor en distancia ............ " ++ veredicto (rDistancia r) (empDist r))
      putStrLn ("    mejor en costo ................ " ++ veredicto (rCosto r) (empCosto r))
      putStrLn ("    w1·d1 + w2·c1 = " ++ binario (m1 r) ++ " (" ++ show (deBits (m1 r)) ++ ")"
                ++ "    w1·d2 + w2·c2 = " ++ binario (m2 r) ++ " (" ++ show (deBits (m2 r)) ++ ")")
      putStrLn ("    mejor con la métrica compuesta  " ++ veredicto (rCompuesta r) (empComp r)
                ++ "   ← RECOMENDACIÓN")

-- Compara el circuito con la aritmética entera.
esCorrecto1 :: (Int, Int, Int, Int, Int, Int) -> Bool
esCorrecto1 (d1, c1, d2, c2, w1, w2) =
  rDistancia r == elige (d1 > d2)
    && rCosto r == elige (c1 > c2)
    && rCompuesta r == elige (q1 > q2)
    && deBits (m1 r) == q1
    && deBits (m2 r) == q2
  where
    r  = recomendarRuta d1 c1 d2 c2 w1 w2
    q1 = w1 * d1 + w2 * c1
    q2 = w1 * d2 + w2 * c2
    elige b = if b then R2 else R1

pruebasEjercicio1 :: IO ()
pruebasEjercicio1 = do
  putStrLn "Pruebas automáticas (el circuito contra la aritmética de Int):"
  let rango6 = [0 .. 63]
  verifica "sumador de 6 bits: a + b correcto en los 4096 pares"
           (and [deBits (sumador (aBits 6 a) (aBits 6 b)) == a + b | a <- rango6, b <- rango6])
  verifica "comparador de 6 bits: (<, =, >) correcto en los 4096 pares"
           (and [comparador (aBits 6 a) (aBits 6 b) == (a < b, a == b, a > b) | a <- rango6, b <- rango6])
  verifica "multiplicador: w · d correcto para w = 1..8 y d = 1..32 (256 casos)"
           (and [ deBits (multiplica (termometro (aBits anchoPeso w))
                                     (extiende anchoMetrica (aBits anchoEntrada d))) == w * d
                | w <- [1 .. 8], d <- [1 .. 32] ])
  let n       = 250
      ds1     = aleatorios 11 n 1 32
      cs1     = aleatorios 22 n 1 32
      ds2     = aleatorios 33 n 1 32
      cs2     = aleatorios 44 n 1 32
      ws1     = aleatorios 55 n 1 8
      ws2     = aleatorios 66 n 1 8
      azar    = [ (d1, c1, d2, c2, w1, w2)
                | (d1, c1, (d2, c2, (w1, w2))) <- zip3 ds1 cs1 (zip3 ds2 cs2 (zip ws1 ws2)) ]
      esquinas = [ (1, 1, 1, 1, 1, 1), (32, 32, 32, 32, 8, 8), (32, 32, 1, 1, 8, 8)
                 , (1, 1, 32, 32, 8, 8), (1, 32, 32, 1, 8, 1), (1, 32, 32, 1, 1, 8) ]
  verifica ("decisión completa (3 respuestas y métricas) en " ++ show (n + length esquinas)
            ++ " casos: " ++ show n ++ " pseudoaleatorios y las esquinas del rango")
           (all esCorrecto1 (esquinas ++ azar))

ejercicio1 :: IO ()
ejercicio1 = do
  titulo "EJERCICIO 1 — Decidiendo caminos"
  putStrLn "Circuito: comparadores en cascada + multiplicadores por sumas repetidas +"
  putStrLn "sumador binario.  Se recomienda R1 salvo que R2 sea estrictamente mejor."
  putStrLn ""
  detalleMultiplicador 3 25
  putStrLn ""
  putStrLn "Casos de ejemplo:"
  mapM_ muestraCaso
    [ (10, 20, 15, 12, 2, 3)    -- Cada criterio simple apunta a una ruta distinta.
    , (5, 5, 5, 5, 3, 4)        -- Empate tota.
    , (32, 32, 1, 1, 8, 8)      -- Extremos, la métrica llega a 512 (10 bits).
    , (1, 32, 32, 1, 8, 1)      -- Pesa más la distancia.
    , (1, 32, 32, 1, 1, 8)      -- Mismos datos, pesa más el costo, entonces cambia la decisión.
    , (0, 5, 3, 4, 2, 9)        -- Entradas fuera de los supuestos.
    ]
  putStrLn ""
  pruebasEjercicio1

-- ==========================
--  2.- Validando votaciones.
-- ==========================
--
--  Entradas. Tres votos J1, J2, J3 (1 = candidato A, 0 = candidato B) y los puntajes Pa y Pb de la empresa externa (enteros no negativos en binario, ambos del mismo ancho; aquí 8 bits, es decir 0...255).
--
--  Diseño del circuito:
--  
--    J1,J2,J3 ──▶ [ CONTADOR ] ──▶ Ca  (votos para A, 2 bits)
--    ¬J1,¬J2,¬J3 ▶ [ CONTADOR ] ──▶ Cb  (votos para B, 2 bits)
--    (Ca, Cb) ──▶ [ COMPARADOR ] ──▶ gana A (Ca > Cb) / gana B (Cb > Ca)
--    (Pa, Pb) ──▶ [ COMPARADOR ] ──▶ Pa > Pb / Pb > Pa / empate
--    (Pa, Pb) ──▶ [ COMPARADOR + MULTIPLEXOR ] ──▶ máx(Pa, Pb)
--    valido = (ganaA_jurados ∧ Pa > Pb) ∨ (ganaB_jurados ∧ Pb > Pa)
--    revisar = ¬valido
--
--  * Contador binario donde cada entrada se suma, con el sumador binario, al acumulador (que parte de 0).
--    Para 3 entradas es un contador de unos de 2 bits: Ca = (maj(J1,J2,J3), J1 ⊕ J2 ⊕ J3) (un sumador completo). Cb cuenta los ceros: es el mismo contador aplicado a los votos negados.
--  * Con 3 jurados Ca + Cb = 3 es impar, así que nunca hay empate de votos.
--  * Si Pa = Pb no hay un candidato con puntaje máximo único, por tanto, no se puede validar y se recomienda revisar el caso.

anchoPuntaje :: Int
anchoPuntaje = 8

-- Bits necesarios para representar n ≥ 1:  3 → 2,  8 → 4.
anchoPara :: Int -> Int
anchoPara n = length (takeWhile (> 0) (iterate (`div` 2) n))

-- Contador binario. Número de entradas que valen 1.
contadorBinario :: [Bit] -> Binario
contadorBinario entradas = foldl suma (replicate w False) entradas
  where
    w = anchoPara (length entradas)
    suma acc b = fst (sumadorN False acc (b : replicate (w - 1) False))

-- (Ca, Cb): votos para A (unos) y votos para B (ceros).
votos :: [Bit] -> (Binario, Binario)
votos js = (contadorBinario js, contadorBinario (map notG js))

data Resultado2 = Resultado2
  { votosA        :: Binario   -- Ca
  , votosB        :: Binario   -- Cb
  , juecesGanaA   :: Bit       -- Ca > Cb
  , juecesGanaB   :: Bit       -- Cb > Ca
  , maxPuntaje    :: Binario   -- máx(Pa, Pb)
  , puntajeGanaA  :: Bit       -- Pa > Pb
  , puntajeGanaB  :: Bit       -- Pb > Pa
  , empatePuntaje :: Bit       -- Pa = Pb
  , valido        :: Bit       -- Los dos criterios señalan al mismo candidato.
  }

validarVotacion :: Bit -> Bit -> Bit -> Binario -> Binario -> Resultado2
validarVotacion j1 j2 j3 pa pb = Resultado2
  { votosA        = ca
  , votosB        = cb
  , juecesGanaA   = jA
  , juecesGanaB   = jB
  , maxPuntaje    = maximoN pa pb
  , puntajeGanaA  = pA
  , puntajeGanaB  = pB
  , empatePuntaje = igualA pa pb
  , valido        = orG (andG jA pA) (andG jB pB)
  }
  where
    (ca, cb) = votos [j1, j2, j3]
    jA = mayorQue ca cb
    jB = mayorQue cb ca
    pA = mayorQue pa pb
    pB = mayorQue pb pa

quienGana :: Bit -> Bit -> String
quienGana a b
  | a         = "A"
  | b         = "B"
  | otherwise = "nadie (empate)"

-- Tabla de verdad del contador de votos (8).
combinaciones3 :: [[Bit]]
combinaciones3 = [[a, b, c] | a <- [False, True], b <- [False, True], c <- [False, True]]

filaVotos :: [Bit] -> String
filaVotos js =
  "    " ++ intercalate "  " (map (\b -> [bitChar b]) js)
    ++ " | " ++ binario ca ++ " " ++ binario cb
    ++ " | " ++ quienGana (mayorQue ca cb) (mayorQue cb ca)
  where
    (ca, cb) = votos js

muestraVotacion :: (Bit, Bit, Bit, Int, Int) -> IO ()
muestraVotacion (j1, j2, j3, pa, pb) = do
  let r = validarVotacion j1 j2 j3 (aBits anchoPuntaje pa) (aBits anchoPuntaje pb)
  putStrLn ("• J1, J2, J3 = " ++ intercalate ", " (map (\b -> [bitChar b]) [j1, j2, j3])
            ++ "    Pa = " ++ show pa ++ ",  Pb = " ++ show pb)
  putStrLn ("    Ca = " ++ binario (votosA r) ++ " (" ++ show (deBits (votosA r)) ++ ")"
            ++ "    Cb = " ++ binario (votosB r) ++ " (" ++ show (deBits (votosB r)) ++ ")"
            ++ "    → ganan los jurados: " ++ quienGana (juecesGanaA r) (juecesGanaB r))
  putStrLn ("    máx(Pa, Pb) = " ++ binario (maxPuntaje r) ++ " (" ++ show (deBits (maxPuntaje r)) ++ ")"
            ++ "    → mayor puntaje: " ++ quienGana (puntajeGanaA r) (puntajeGanaB r))
  putStrLn ("    resultado: "
            ++ (if valido r
                  then "VALIDADO (los jurados y la empresa externa coinciden)"
                  else "REVISAR EL CASO (no coinciden"
                       ++ (if empatePuntaje r then "; los puntajes están empatados)" else ")")))

-- Compara el circuito con la aritmética entera.
esCorrecto2 :: [Bit] -> Int -> Int -> Bool
esCorrecto2 [j1, j2, j3] pa pb =
  valido r == esperado
    && deBits (maxPuntaje r) == max pa pb
    && empatePuntaje r == (pa == pb)
  where
    r        = validarVotacion j1 j2 j3 (aBits 4 pa) (aBits 4 pb)
    juecesA  = length (filter id [j1, j2, j3]) >= 2
    esperado = (juecesA && pa > pb) || (not juecesA && pb > pa)
esCorrecto2 _ _ _ = False

pruebasEjercicio2 :: IO ()
pruebasEjercicio2 = do
  putStrLn "Pruebas automáticas (el circuito contra la aritmética de Int):"
  let cuentaUnos js = length (filter id js)
  verifica "contador: Ca = número de votos 1, Cb = número de votos 0 y Ca + Cb = 3 (8 combinaciones)"
           (and [ deBits ca == cuentaUnos js && deBits cb == 3 - cuentaUnos js
                | js <- combinaciones3, let (ca, cb) = votos js ])
  verifica "validación correcta en 8 combinaciones de votos × 256 pares de puntajes de 4 bits (2048 casos)"
           (and [esCorrecto2 js pa pb | js <- combinaciones3, pa <- [0 .. 15], pb <- [0 .. 15]])

ejercicio2 :: IO ()
ejercicio2 = do
  titulo "EJERCICIO 2 — Validando votaciones"
  putStrLn "Contador binario de votos (1 = voto para A, 0 = voto para B):"
  putStrLn "   J1 J2 J3 | Ca Cb | ganan los jurados"
  mapM_ (putStrLn . filaVotos) combinaciones3
  putStrLn ""
  putStrLn ("Casos de ejemplo (puntajes de " ++ show anchoPuntaje ++ " bits):")
  mapM_ muestraVotacion
    [ (True, True, False, 87, 64)     -- Jurados: A;  empresa: A  → coinciden.
    , (False, False, True, 87, 64)    -- Jurados: B;  empresa: A  → no coinciden.
    , (True, False, True, 70, 70)     -- Jurados: A;  puntajes empatados.
    , (False, True, False, 12, 200)   -- Jurados: B;  empresa: B  → coinciden.
    ]
  putStrLn ""
  pruebasEjercicio2

-- =======================================================
--  3.- Filtro de imágenes binarias: apertura morfológica.
-- =======================================================
--
--  Una imagen es un arreglo bidimensional de bits (lista de renglones, de arriba hacia abajo). 
--  Una posición es (x, y): x = columna, y = renglón, ambos desde 0. Fuera de la imagen se supone fondo (0).
--
--  Elemento estructurante  E  (cruz 3×3), con E(i, j): i = columna, j = renglón:
--
--         0 1 0
--         1 1 1
--         0 1 0
--
--  Erosión en (x0, y0)  =  mín de los cinco términos  I(vecino) ∧ E(i, j). (El "mín" de bits es el AND de todos).
--  Dilatación en (x0, y0) = máx de los cinco términos  I(vecino) ∧ E(i, j). (El "máx" de bits es el OR de todos).
--
--  Según con el enunciado: la dilatación aparece escrita con  I(...) ∨ E(...).
--  Como las cinco entradas de E que se usan valen 1, cada término sería 1 y la dilatación daría SIEMPRE 1 (la imagen quedaría completamente blanca; ya no sería un filtro).  
--  La dilatación morfológica estándar (la que hace útil la apertura) usa ∧.  Así "dilatacionEn" usa ∧; "dilatacionLiteralEn" es la fórmula tal como está escrita, sólo para comparar.
--
--  Apertura = (a) erosión de I en las posiciones p, guardada en I'; luego (b) dilatación de I' en las posiciones p, guardada en un archivo de texto (un renglón de la imagen por línea, valores separados por comas).
--  Las posiciones que no están en p quedan en 0 en el resultado.

type Imagen   = [[Bit]]
type Pos      = (Int, Int)
type Elemento = [[Bit]]

elementoCruz :: Elemento
elementoCruz = [ [False, True,  False]
               , [True,  True,  True ]
               , [False, True,  False] ]

-- E(i, j) con i = columna y j = renglón (1, 2, 3), como en el enunciado.
enE :: Elemento -> Int -> Int -> Bit
enE e i j = e !! (j - 1) !! (i - 1)

alto :: Imagen -> Int
alto = length

ancho :: Imagen -> Int
ancho []      = 0
ancho (r : _) = length r

-- I(x, y); fuera de la imagen vale 0.
pixel :: Imagen -> Pos -> Bit
pixel im (x, y)
  | y < 0 || y >= alto im  = False
  | x < 0 || x >= length r = False
  | otherwise              = r !! x
  where
    r = im !! y

-- Imagen de h renglones por w columnas, toda en 0.
ceros :: Int -> Int -> Imagen
ceros h w = replicate h (replicate w False)

-- Todas las posiciones de la imagen, renglón por renglón.
todasLasPosiciones :: Imagen -> [Pos]
todasLasPosiciones im = [(x, y) | y <- [0 .. alto im - 1], x <- [0 .. ancho im - 1]]

-- Cambia el elemento i de una lista.
cambiaEn :: Int -> a -> [a] -> [a]
cambiaEn i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- "Almacena" un valor en una posición (devuelve la imagen actualizada).
poneEn :: Imagen -> (Pos, Bit) -> Imagen
poneEn im ((x, y), v)
  | x >= 0 && y >= 0 && y < alto im && x < ancho im =
      [if j == y then cambiaEn x v fila else fila | (j, fila) <- zip [0 ..] im]
  | otherwise = im

-- Almacena varios valores en un arreglo.
guarda :: Imagen -> [(Pos, Bit)] -> Imagen
guarda = foldl' poneEn

-- "mín" y "máx" de una lista de bits.
minBits :: [Bit] -> Bit
minBits = foldr andG True

maxBits :: [Bit] -> Bit
maxBits = foldr orG False

-- Los cinco términos de las fórmulas del enunciado.
-- Desplazamiento en la imagen y entrada correspondiente de E.
vecindad :: [((Int, Int), (Int, Int))]
vecindad =
  [ (( 0,  0), (2, 2))    -- I(x0,   y0)    con E(2,2)
  , (( 1,  0), (3, 2))    -- I(x0+1, y0)    con E(3,2)
  , ((-1,  0), (1, 2))    -- I(x0-1, y0)    con E(1,2)
  , (( 0,  1), (2, 3))    -- I(x0,   y0+1)  con E(2,3)
  , (( 0, -1), (2, 1))    -- I(x0,   y0-1)  con E(2,1)
  ]

-- Los cinco términos I(vecino) ∧ E(i, j)  alrededor de (x0, y0).
terminos :: Elemento -> Imagen -> Pos -> [Bit]
terminos e im (x0, y0) =
  [andG (pixel im (x0 + dx, y0 + dy)) (enE e i j) | ((dx, dy), (i, j)) <- vecindad]

-- Erosión binaria en una posición. mín de los términos.
erosionEn :: Elemento -> Imagen -> Pos -> Bit
erosionEn e im q = minBits (terminos e im q)

-- Dilatación binaria en una posición. máx de los términos I ∧ E.
dilatacionEn :: Elemento -> Imagen -> Pos -> Bit
dilatacionEn e im q = maxBits (terminos e im q)

-- La dilatación escrita tal cual en el enunciado (I ∨ E): siempre vale 1.
dilatacionLiteralEn :: Elemento -> Imagen -> Pos -> Bit
dilatacionLiteralEn e im (x0, y0) =
  maxBits [orG (pixel im (x0 + dx, y0 + dy)) (enE e i j) | ((dx, dy), (i, j)) <- vecindad]

-- (a)... Erosión de I en las posiciones p; el resultado se almacena en I'.
erosiona :: Elemento -> Imagen -> Imagen -> [Pos] -> Imagen
erosiona e im im' ps = guarda im' [(q, erosionEn e im q) | q <- ps]

-- (b)... Dilatación de I' en las posiciones p; el resultado se almacena en "salida".
dilata :: Elemento -> Imagen -> Imagen -> [Pos] -> Imagen
dilata e im' salida ps = guarda salida [(q, dilatacionEn e im' q) | q <- ps]

-- Las dos etapas sin escribir archivo. Se devuelve (I', resultado).
etapas :: Elemento -> Imagen -> Imagen -> [Pos] -> (Imagen, Imagen)
etapas e im im' ps = (ero, res)
  where
    ero = erosiona e im im' ps
    res = dilata e ero (ceros (alto im) (ancho im)) ps

-- Un renglón como "0,1,1,0".
renglonCsv :: [Bit] -> String
renglonCsv = intercalate "," . map (\b -> [bitChar b])

-- La imagen como texto. Un renglón por línea, valores separados por comas.
imagenACsv :: Imagen -> String
imagenACsv = unlines . map renglonCsv

-- Lee una imagen de un texto con 0/1 separados por comas (un renglón por línea).
csvAImagen :: String -> Imagen
csvAImagen = map (map (== '1') . filter (`elem` "01")) . filter (any (`elem` "01")) . lines

-- Apertura morfológica completa. 
-- Entradas: archivo de salida, E, I, el arreglo I' donde se guarda la erosión y la lista p de posiciones.  
-- Devuelve (I', resultado).
apertura :: FilePath -> Elemento -> Imagen -> Imagen -> [Pos] -> IO (Imagen, Imagen)
apertura ruta e im im' ps = do
  let (ero, res) = etapas e im im' ps
  writeFile ruta (imagenACsv res)
  return (ero, res)

-- -----------------------------------
--  Dibujo en pantalla y demostración.
-- -----------------------------------

-- X = 1,  . = 0
dibuja :: Imagen -> [String]
dibuja = map (unwords . map (\b -> if b then "X" else "."))

-- Varias imágenes (del mismo tamaño) una junto a otra, cada una con su título.
ladoALado :: [(String, Imagen)] -> [String]
ladoALado cols = titulos : cuerpo
  where
    bloques = [dibuja im | (_, im) <- cols]
    anchoB  = maximum (0 : map length (map fst cols ++ concat bloques))
    titulos = intercalate "   " [rellena anchoB t | (t, _) <- cols]
    cuerpo  = [intercalate "   " (map (rellena anchoB) fila) | fila <- transpose bloques]

-- Número de píxeles en 1.
cuenta :: Imagen -> Int
cuenta = length . filter id . concat

leeDibujo :: [String] -> Imagen
leeDibujo = map (map (== 'X'))

-- Ejemplito de imagen de muestra (12 columnas × 10 renglones), donde:
--   *Ruido aislado, una mancha de 2 píxeles y una línea de 1 píxel de ancho (se eliminan).
--   *Un rombo y una cruz (sobreviven intactos).
--   *Un rectángulo de 4×3 (sobrevive sin sus 4 esquinas: la cruz no cabe en ellas).
imagenMuestra :: Imagen
imagenMuestra = leeDibujo
  [ "........X..X"
  , "...X....X..."
  , "..XXX...X..."
  , ".XXXXX..X.X."
  , "..XXX......."
  , "...X...XXXX."
  , "..X....XXXX."
  , ".XXX...XXXX."
  , "..X........."
  , ".........XX."
  ]

-- Imagen pseudoaleatoria de h renglones por w columnas.
troceaEn :: Int -> [a] -> [[a]]
troceaEn _ [] = []
troceaEn n xs = take n xs : troceaEn n (drop n xs)

imagenAleatoria :: Int -> Int -> Int -> Imagen
imagenAleatoria semilla h w = troceaEn w (take (h * w) bits)
  where
    bits = [(s `div` 65536) `mod` 100 < 55 | s <- tail (iterate siguiente semilla)]

-- Definiciones directas para las pruebas. Los cuatro vecinos y el centro.
vecinos4 :: Pos -> [Pos]
vecinos4 (x, y) = [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Apertura de toda la imagen (sin archivo).
aperturaPura :: Imagen -> (Imagen, Imagen)
aperturaPura im = etapas elementoCruz im (ceros (alto im) (ancho im)) (todasLasPosiciones im)

cumpleErosion :: Imagen -> Bool
cumpleErosion im =
  and [pixel ero q == all (pixel im) (vecinos4 q) | q <- todasLasPosiciones im]
  where
    ero = fst (aperturaPura im)

cumpleDilatacion :: Imagen -> Bool
cumpleDilatacion im =
  and [pixel res q == any (pixel ero) (vecinos4 q) | q <- todasLasPosiciones im]
  where
    (ero, res) = aperturaPura im

-- La apertura nunca agrega píxeles: resultado ⊆ I.
esAntiextensiva :: Imagen -> Bool
esAntiextensiva im =
  and [not (pixel res q) || pixel im q | q <- todasLasPosiciones im]
  where
    res = snd (aperturaPura im)

-- Aplicar la apertura dos veces equivale a aplicarla una vez.
esIdempotente :: Imagen -> Bool
esIdempotente im = snd (aperturaPura primera) == primera
  where
    primera = snd (aperturaPura im)

pruebasEjercicio3 :: IO ()
pruebasEjercicio3 = do
  putStrLn "Pruebas automáticas (imagen de muestra y 40 imágenes pseudoaleatorias de 7×8):"
  let imagenes = imagenMuestra : [imagenAleatoria s 7 8 | s <- [1 .. 40]]
  verifica "la erosión coincide con la definición directa (AND de los 5 píxeles de la cruz)"
           (all cumpleErosion imagenes)
  verifica "la dilatación coincide con la definición directa (OR de los 5 píxeles de la cruz)"
           (all cumpleDilatacion imagenes)
  verifica "la apertura es anti-extensiva: no agrega píxeles que no estaban en I"
           (all esAntiextensiva imagenes)
  verifica "la apertura es idempotente: aplicarla dos veces = aplicarla una vez"
           (all esIdempotente imagenes)

verificaArchivo :: FilePath -> Imagen -> IO ()
verificaArchivo ruta esperado = do
  contenido <- readFile ruta
  let leida = csvAImagen contenido
  verifica ("el archivo " ++ ruta ++ " se volvió a leer: " ++ show (alto leida) ++ " renglones de "
            ++ show (ancho leida) ++ " valores 0/1 separados por comas, igual al resultado")
           (leida == esperado)
  case lines contenido of
    (primera : _) -> putStrLn ("    primer renglón del archivo: " ++ primera)
    []            -> return ()

-- Aplica la apertura a una imagen dada en un archivo CSV (0/1 separados por comas).
filtraArchivo :: FilePath -> IO ()
filtraArchivo entrada = do
  contenido <- readFile entrada
  let im     = csvAImagen contenido
      salida = entrada ++ ".apertura.txt"
  if null im || ancho im == 0
    then putStrLn ("  ✗ no pude leer una imagen en " ++ entrada)
    else do
      (_, res) <- apertura salida elementoCruz im (ceros (alto im) (ancho im)) (todasLasPosiciones im)
      putStrLn ("  imagen de " ++ show (ancho im) ++ " columnas × " ++ show (alto im) ++ " renglones: "
                ++ show (cuenta im) ++ " píxeles en 1 → " ++ show (cuenta res) ++ " después de la apertura")
      putStrLn ("  resultado guardado en " ++ salida)

ejercicio3 :: [String] -> IO ()
ejercicio3 args = do
  titulo "EJERCICIO 3 — Filtro de imágenes binarias (apertura morfológica)"
  putStrLn "Elemento estructurante E (cruz 3×3):"
  mapM_ (putStrLn . ("    " ++) . unwords . map (\b -> [bitChar b])) elementoCruz
  putStrLn ""
  let im = imagenMuestra
      h  = alto im
      w  = ancho im
      ps = todasLasPosiciones im
  (ero, res) <- apertura "apertura.txt" elementoCruz im (ceros h w) ps
  putStrLn ("Imagen de muestra, " ++ show w ++ " columnas × " ++ show h ++ " renglones (X = 1, . = 0),"
            ++ " p = las " ++ show (length ps) ++ " posiciones:")
  mapM_ putStrLn (ladoALado [("I", im), ("I' = erosión (a)", ero), ("dilatación de I' (b)", res)])
  putStrLn ("  píxeles en 1:  I = " ++ show (cuenta im) ++ ",  I' = " ++ show (cuenta ero)
            ++ ",  resultado = " ++ show (cuenta res))
  putStrLn "  Desaparecen el ruido aislado, la mancha de 2 píxeles y la línea de 1 píxel de ancho;"
  putStrLn "  el rombo y la cruz quedan intactos; el rectángulo pierde sus 4 esquinas."
  putStrLn ""
  let literal = guarda (ceros h w) [(q, dilatacionLiteralEn elementoCruz ero q) | q <- ps]
  putStrLn ("Con la dilatación escrita tal cual en el enunciado (I ∨ E) quedarían en 1 "
            ++ show (cuenta literal) ++ " de " ++ show (h * w) ++ " píxeles: no filtra nada.")
  putStrLn ""
  let psIzq  = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w `div` 2 - 1]]
      (eroIzq, resIzq) = etapas elementoCruz im (ceros h w) psIzq
  putStrLn ("Mismo filtro, pero sólo en las " ++ show (length psIzq)
            ++ " posiciones de la mitad izquierda (x < " ++ show (w `div` 2) ++ "):")
  mapM_ putStrLn (ladoALado [("I", im), ("I' = erosión (a)", eroIzq), ("dilatación de I' (b)", resIzq)])
  putStrLn ""
  verificaArchivo "apertura.txt" res
  putStrLn ""
  pruebasEjercicio3
  case args of
    (entrada : _) -> do
      putStrLn ""
      putStrLn ("Aplicando la apertura a tu imagen " ++ entrada ++ ":")
      filtraArchivo entrada
    [] -> return ()

-- =========================
--  Programa listo k listo.
-- =========================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  ejercicio1
  ejercicio2
  ejercicio3 args
  putStrLn ""
