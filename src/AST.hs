{-# LANGUAGE GADTs #-}
module AST where

-- Identificadores para los qubits.
type Qubit = Int

-- Expresiones de Compuertas Cuánticas (Deep Embedding).
data Gate where
    -- Compuertas de 1 Qubit.
    H :: Qubit -> Gate              -- Hadamard
    X :: Qubit -> Gate              -- Pauli-X (NOT)

    -- Compuertas de 2 Qubits.
    CNOT :: Qubit -> Qubit -> Gate  -- Controlled-NOT

    -- Compuertas de 3 Qubits.
    CCNOT :: Qubit -> Qubit -> Qubit -> Gate    -- Toffoli

    -- Combinadores
    Seq :: Gate -> Gate -> Gate     -- Secuenciación de compuertas
    Par :: Gate -> Gate -> Gate     -- Aplicación en paralelo (Producto Tensorial)
    deriving Show

-- Estructura del Circuito.
--   - numQubits: define la dimensión del espacio de Hilbert (2^n).
--   - gates: es el AST que describe qué operaciones realizar y en qué orden.
data Circuit = Circuit {
    numQubits :: Int,
    gates :: Gate
} deriving (Show)

-- Representación de estados para el Pretty-Printer
type DiracNotation = String