module PrettyPrinter where

{- 
Este módulo se encarga de la presentación de resultados.
Transforma los objetos internos (Circuitos, Vectores de Estado) en 
representaciones textuales legibles por humanos (notación de Dirac, binario, etc).
-}

import AST
import Eval
import Data.Complex
import Data.Bits
import Data.List (intercalate)
import Text.Printf (printf)

-- intToBinary: Convierte un índice del StateVector a su representación binaria.
-- Argumentos:
--   - n: Cantidad total de qubits (determina el largo del string).
--   - i: El índice decimal del estado.
-- Retorna: Un String como "101" donde el bit 0 (LSB) está a la derecha.
intToBinary :: Int -> Int -> String
intToBinary n i = [ if testBit i k then '1' else '0' | k <- [n-1, n-2 .. 0]]


-- formatComplex: Redondea y da formato a amplitudes complejas.
--   - Limpia casos comunes (1.0, -1.0) para que no ensucien la salida.
--   - Oculta partes reales o imaginarias si son despreciables (< 1e-10).
--   - Retorna: String con formato "a + bi".
formatComplex :: Complex Double -> String
formatComplex (re :+ im)
    | abs im < 1e-10 && abs (re - 1) < 1e-10 = ""
    | abs im < 1e-10 && abs (re + 1) < 1e-10 = "-"
    | abs im < 1e-10 = printf "%.2f" re
    | abs re < 1e-10 = printf "%.2fi" im
    | im > 0 = printf "(%.2f + %.2fi)" re im
    | otherwise = printf "(%.2f - %.2fi)" re (abs im)


-- prettyPrint: Transforma el StateVector en una suma de escalares multiplicados por estados.
-- Solo incluye los estados que tienen una probabilidad distinta de cero.
-- Ejemplo: "0.71|00> + 0.71|11>"
--
-- Argumentos:
--   - n: Cantidad total de qubits del sistema.
--   - sv: El vector de estado (StateVector) con las amplitudes complejas.
-- Retorna: Una cadena con la representación en notación de Dirac del estado.
prettyPrint :: Int -> StateVector -> String
prettyPrint n sv = 
    let -- 1. Obtenemos una lista de tuplas (indice, amplitud).
        indicesEstados = zip [0..] sv
        -- 2. Filtramos amplitudes nulas.
        estadosActivos = filter (\(_, v) -> magnitude v > 1e-10) indicesEstados
        -- 3. Función local para formatear cada estado.
        formatTerm (i, v) = formatComplex v ++ "|" ++ intToBinary n i ++ ">"
        -- 4. Unimos todos los términos con el signo '+'.
        terms = map formatTerm estadosActivos
    in if null terms then "|0>" else intercalate " + " terms


-- ppGate: Convierte la estructura de datos Gate en una cadena legible.
ppGate :: Gate -> String
ppGate (H q) = "H(" ++ show q ++ ")"
ppGate (X q) = "X(" ++ show q ++ ")"
ppGate (CNOT c t) = "CNOT(" ++ show c ++ "," ++ show t ++ ")"
ppGate (CCNOT c1 c2 t) = "CCNOT(" ++ show c1 ++ "," ++ show c2 ++ "," ++ show t ++ ")"
ppGate (Seq g1 g2) = ppGate g1 ++ " ; " ++ ppGate g2
ppGate (Par g1 g2) = "(" ++ ppGate g1 ++ " || " ++ ppGate g2 ++ ")"


-- ppCircuit: Muestra la cabecera del circuito y su cuerpo.
ppCircuit :: Circuit -> String
ppCircuit (Circuit n g) = "Circuito[" ++ show n ++ " qubits]: " ++ ppGate g