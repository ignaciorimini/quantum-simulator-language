// Algoritmo de Grover (2 qubits)
// El objetivo es encontrar el estado "ganador" |11>
qubits 2

// Ponemos ambos qubits en superposición (0.5|00> + 0.5|01> + 0.5|10> + 0.5|11>)
(H 0 || H 1) ;

// Invierte el signo del estado |11> (lo "marca"). 
// H ; CNOT ; H equivale a una compuerta CZ (Controlled-Z)
(H 1 ; CNOT 0 1 ; H 1) ;

// Amplifica la amplitud del estado marcado y reduce las otras.
(H 0 || H 1) ;
(X 0 || X 1) ;
(H 1 ; CNOT 0 1 ; H 1) ;
(X 0 || X 1) ;
(H 0 || H 1)
