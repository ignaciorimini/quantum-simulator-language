// Medio Sumador Cuántico (Half-Adder)
// Entradas: Q3 y Q2.
// Salidas: Q1 (Acarreo) y Q0 (Suma).
qubits 4

// --- PASO 1: Inicializar entradas ---
// Probamos con 1 + 1 (debería dar Suma=0, Acarreo=1 -> "10" en binario)
X 3 ; X 2; 

// --- PASO 2: Cálculo del Acarreo (AND) ---
// Si Q3=1 y Q2=1, invierte Q1 (pasa de 0 a 1).
CCNOT 3 2 1 ;

// --- PASO 3: Cálculo de la Suma (XOR) ---
// Aplicamos XOR de Q3 y Q2 sobre el qubit de salida Q0.
CNOT 3 0 ;
CNOT 2 0
