module Examples where

import AST

-- Estado de Bell: |00> + |11>
-- Aplica la compuerta Hadamard (H) al qubit 1 y la compuerta CNOT con control en 1 y objetivo en 0.
bellState :: Circuit
bellState = Circuit 2 (Seq (H 1) (CNOT 1 0))

-- Estado GHZ: |000> + |111>
ghzState :: Circuit
ghzState = Circuit 3 (Seq (H 2) (Seq (CNOT 2 1) (CNOT 1 0)))

-- Ejemplo de compuertas en paralelo.
ejemploParalelo :: Circuit
ejemploParalelo = Circuit 2 (Par (H 0) (X 1))

-- Superposición total en 2 qubits: 0.50|00> + 0.50|01> + 0.50|10> + 0.50|11>
superposicionTotal :: Circuit
superposicionTotal = Circuit 2 (Par (H 0) (H 1))

-- Estado con signo negativo: 0.71|0> - 0.71|1>
-- Primero aplicamos X para estar en |1>, luego H.
estadoMenos :: Circuit
estadoMenos = Circuit 1 (Seq (X 0) (H 0))