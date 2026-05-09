module Eval where

-- CONVENCIÓN DE QUBITS:
-- El qubit 0 es el bit menos significativo (LSB, más a la derecha en binario).
-- El qubit n-1 es el bit más significativo (MSB, más a la izquierda en binario).
-- Ejemplo: en un sistema de 3 qubits, el estado base i=4 (binario 100) representa:
--   - Qubit 0 = 0 (el bit de peso 2^0 es 0)
--   - Qubit 1 = 0 (el bit de peso 2^1 es 0)
--   - Qubit 2 = 1 (el bit de peso 2^2 es 1)
-- Esto significa que el estado sería |100⟩, donde el 1 corresponde al qubit 2.

-- Módulo Data.Complex:
-- Provee el tipo 'Complex' y el constructor ':+' para manejar números complejos (a + bi).

-- Módulo Data.List:
-- - transpose: Cambia filas por columnas. Se usa para multiplicar matrices (A*B),
--   permitiendo hacer productos escalares entre las filas de A y las columnas de B.
-- - intersect: Calcula la intersección de dos listas. Lo usamos para validar que
--   dos compuertas en paralelo no operen sobre los mismos qubits.

-- Módulo Data.Bits:
-- Provee operaciones a nivel de bits sobre enteros:
-- - testBit: Verifica si el n-ésimo bit de un número es 0 o 1 (ej. estado del qubit n).
-- - xor: Operación Or-Exclusivo. Se usa en CNOT para invertir el bit objetivo.
-- - shiftL: Desplazamiento a la izquierda (shl). '1 `shiftL` n' genera el valor 2^n.

import AST
import Data.Complex
import Data.List (transpose, intersect)
import Data.Bits (testBit, xor, shiftL)


------------------------------------------------
-- 1. Definición de la Mónada QState
------------------------------------------------

-- Definición de los errores posibles durante la evaluación.
-- 1. QubitOutOufBounds: el usuario intentó acceder a un qubit que no existe.
-- 2. InvalidCircuit: error estructural en el circuito.
-- 3. DimensionMismatch: las matrices no coinciden con el tamaño del vector.
-- 4. TooManyQubits: límite preventivo para evitar el agotamiento de memoria del sistema.
data QError = QubitOutOfBounds Int
	| InvalidCircuit String
	| DimensionMismatch String
	| TooManyQubits Int
    deriving Show

-- Sinónimos de tipo.
type QTrace = [String]                  -- Lista de mensajes en forma de "log".
type Matrix = [[Complex Double]]        -- Representamos matrices como listas de listas.
type StateVector = [Complex Double]     -- Representación del vector de estado del sistema (|ψ>).

-- QState a :: (StateVector -> Either QError (a, StateVector, QTrace)) -> QState a
-- runQState :: QState a -> (StateVector -> Either QError (a, StateVector, QTrace))
-- QState es una función que:
-- 1. Recibe el estado actual (StateVector).
-- 2. Devuelve un Either:
--    - Left QError: si algo falló, se corta la ejecución y devuelve el error.
--    - Right (a, StateVector, QTrace): si tuvo éxito, devuelve:
--      * 'a': el resultado de la computación (puede ser () si solo modificamos el estado).
--      * 'StateVector': el nuevo estado del sistema después de aplicar la compuerta.
--      * 'QTrace': los mensajes acumulados de lo que se fue haciendo.
newtype QState a = QState {
	runQState :: StateVector -> Either QError (a, StateVector, QTrace)
}

-- Instancia de Functor
-- Permite aplicar una función al resultado 'a' sin tocar el estado ni la traza.
instance Functor QState where
	-- fmap :: (a -> b) -> QState a -> QState b
	fmap f m = QState (\s -> case runQState m s of
		Left e -> Left e
		Right (v, s', t) -> Right (f v, s', t))

-- Instancia de Applicative
instance Applicative QState where
	-- pure :: a -> QState a
	-- Crea una computación que no toca el estado y traza vacía.
	pure x = QState (\s -> Right (x, s, []))

	-- (<*>) :: QState (a -> b) -> QState a -> QState b
	mf <*> ma = QState (\s -> 
		-- Ejecutamos la primera computación (mf) para obtener la función 'f'.
		case runQState mf s of
			Left e1 -> Left e1
			Right (f, s', t1) -> 
				-- Ejecutamos la segunda (ma).
				case runQState ma s' of
					Left e2 -> Left e2
					-- Combinamos todo: aplicamos f a v, devolvemos el último estado
                    -- y concatenamos las trazas.
					Right (v, s'', t2) -> Right (f v, s'', t1 ++ t2))

-- Instancia de Monad
instance Monad QState where
	-- return :: a -> QState a
	return = pure

	-- (>>=) :: QState a -> (a -> QState b) -> QState b
	m >>= f = QState (\s -> 
		case runQState m s of
			Left e1 -> Left e1
			Right (v1, s', t1) -> 
				case runQState (f v1) s' of
					Left e2 -> Left e2
					Right (v2, s'', t2) -> Right (v2, s'', t1 ++ t2))


------------------------------------------------
-- 2. Operaciones Básicas de la Mónada
------------------------------------------------

-- Obtiene el vector de estado actual para poder operar con él.
getState :: QState StateVector
getState = QState (\s -> Right (s, s, []))

-- Reemplaza el estado viejo por uno nuevo.
putState :: StateVector -> QState ()
putState s = QState (\_ -> Right ((), s, []))

-- Lanza un error y termina la computación.
throwQError :: QError -> QState a
throwQError e = QState (\_ -> Left e)

-- Agrega un mensaje a la lista de la traza.
addTrace :: String -> QState ()
addTrace msg = QState (\s -> Right ((), s, [msg]))


------------------------------------------------
-- 3. Definición de compuertas (matrices) base
------------------------------------------------

-- Matriz identidad (2x2).
matI :: Matrix
matI = [[1 :+ 0, 0 :+ 0],
        [0 :+ 0, 1 :+ 0]]

-- Matriz X-NOT (2x2).
matX :: Matrix
matX = [[0 :+ 0, 1 :+ 0],
        [1 :+ 0, 0 :+ 0]]

-- Matrix H (2x2).
matH :: Matrix
matH = let r = 1 / sqrt 2 in
    [[(r :+ 0), (r :+ 0)],
     [(r :+ 0), ((-r) :+ 0)]]


------------------------------------------------
-- 4. Álgebra Lineal y Utilidades
------------------------------------------------

-- initialState :: Int -> StateVector
-- Genera el vector de estado base |00...0> para un sistema de n qubits.
-- Representa el estado inicial estándar en computación cuántica.
-- Argumentos:
--	- n: Cantidad de qubits en el sistema.
-- Devuelve: Un vector de tamaño 2^n con un 1 en la primera posición y el resto en 0.
-- Ejemplo: initialState 2 => [1:+0, 0:+0, 0:+0, 0:+0] (representa el estado |00>)
initialState :: Int -> StateVector
initialState n = (1 :+ 0) : replicate (2^n - 1) (0 :+ 0)

------------------
-- mvecmult :: Matrix -> StateVector -> StateVector
-- Realiza la multiplicación de una matriz compleja por un vector de estado.
-- En mecánica cuántica, esto equivale a aplicar un operador unitario U al estado |ψ>.
-- Argumentos:
--	- m: Matriz que representa la transformación cuántica (2^n x 2^n).
--  - v: Vector de estado actual del sistema.
-- Devuelve: El nuevo StateVector resultante de la aplicación (U|ψ>).
-- Ejemplo: mvecmult matX [1:+0, 0:+0] => [0:+0, 1:+0] (aplica NOT al estado |0>)
mvecmult :: Matrix -> StateVector -> StateVector
mvecmult m v = [ sum (zipWith (*) row v) | row <- m ]

------------------
-- mmult :: Matrix -> Matrix -> Matrix
-- Realiza la multiplicación de dos matrices complejas. Se utiliza para la 
-- composición secuencial de compuertas (combinador Seq).
-- Argumentos:
--	- a: Matriz que se aplica en segundo lugar.
--	- b: Matriz que se aplica en primer lugar.
-- Devuelve: La matriz resultante de la composición (A * B).
mmult :: Matrix -> Matrix -> Matrix
mmult a b = [ [ sum (zipWith (*) row col) | col <- transpose b ] | row <- a ]

------------------
-- tensor :: Matrix -> Matrix -> Matrix
-- Calcula el producto tensorial (o de Kronecker) entre dos matrices.
-- Argumentos:
--	- a: Matriz del primer sistema.
--	- b: Matriz del segundo sistema.
-- Devuelve: Matriz bloque que representa el producto tensorial A ⊗ B.
-- Ejemplo: tensor matI matX => Matriz 4x4 que aplica Identidad al Qubit 1 y NOT al Qubit 0.
tensor :: Matrix -> Matrix -> Matrix
tensor a b = [ [x * y | x <- rowA, y <- rowB] | rowA <- a, rowB <- b]

------------------
-- expandGate :: Int -> Int -> Matrix -> Matrix
-- Expande una compuerta local (de 1 qubit) al espacio de estados global de n qubits
-- rellenando con matrices identidad.
-- Argumentos:
--	- n: Cantidad total de qubits en el circuito.
--	- q: Índice del qubit objetivo (0 a n-1).
--	- mat: Matriz de 2x2 de la compuerta a expandir.
-- Devuelve: Una matriz de 2^n x 2^n que actúa únicamente sobre el qubit 'q'.
-- Ejemplo: expandGate 3 1 matH
--	1. Genera la lista de matrices [matI, matH, matI]
-- 	2. Invierte la lista: [matI, matH, matI] (reverse preserva para comparabilidad con CNOT).
-- 	3. Aplica producto tensorial: matI ⊗ matH ⊗ matI
-- 	4. Resultado: una matriz de 8x8 que aplica Hadamard solo al segundo qubit (índice 1)
-- 	El reverse sirve para colocar el qubit 0 (LSB) al final del producto tensorial, 
-- 	consistente con expandCNOT que usa testBit (convención LSB = qubit 0).
expandGate :: Int -> Int -> Matrix -> Matrix
expandGate n q mat = foldr1 tensor (reverse [if i == q then mat else matI | i <- [0..n-1]])

------------------
-- expandCNOT :: Int -> Int -> Int -> Matrix
-- Construye una matriz CNOT generalizada que actúa en un sistema de n qubits.
-- Utiliza manipulación de bits para determinar si el bit de control está activo
-- y, en ese caso, invierte el bit objetivo.
-- Argumentos:
--	- n: Cantidad total de qubits en el sistema.
--	- c: Índice del qubit de control.
--	- t: Índice del qubit objetivo.
-- Devuelve: Una matriz de 2^n x 2^n.
-- Ejemplo: expandCNOT 2 1 0 => Genera la matriz CNOT estándar de 4x4 donde el qubit 1 controla al 0.
expandCNOT :: Int -> Int -> Int -> Matrix
expandCNOT n c t
  | c < 0 || c >= n = error ("Qubit de control " ++ show c ++ " fuera de rango")
  | t < 0 || t >= n = error ("Qubit objetivo " ++ show t ++ " fuera de rango")
  | c == t = error "Los qubits de control y objetivo deben ser distintos"
  | otherwise = [[cnotElement i j | j <- [0..limit]] | i <- [0..limit]]
  where
    limit = 2^n - 1 :: Int
    -- Determina si la transición de estado i a j es válida según CNOT.
    -- Si el bit de control en i es 1, entonces j debe ser i con el bit objetivo invertido.
    -- Si el bit de control en i es 0, entonces j debe ser igual a i (identidad).
    cnotElement i j =
      if testBit i c  -- Si el bit en posición 'c' de i es 1
        then if j == (i `xor` (1 `shiftL` t)) then 1 :+ 0 else 0 :+ 0  -- Invierte bit en posición t
        else if i == j then 1 :+ 0 else 0 :+ 0  -- Si control es 0, es identidad

------------------
-- expandCCNOT :: Int -> Int -> Int -> Int -> Matrix
-- Construye una matriz Toffoli (CCNOT) generalizada para un sistema de n qubits.
-- Utiliza manipulación de bits para verificar si AMBOS bits de control están activos
-- y, en ese caso, invierte el bit objetivo.
-- Argumentos:
--	- n: Cantidad total de qubits en el sistema.
--	- c1: Índice del primer qubit de control.
--	- c2: Índice del segundo qubit de control.
--	- t: Índice del qubit objetivo.
-- Devuelve: Una matriz de 2^n x 2^n.
-- Ejemplo: expandCCNOT 3 2 1 0 => Genera una matriz de 8x8 donde los qubits 2 y 1 
--          controlan la inversión del qubit 0 (solo si q2=1 y q1=1).
expandCCNOT :: Int -> Int -> Int -> Int -> Matrix
expandCCNOT n c1 c2 t
  | any (\q -> q < 0 || q >= n) [c1, c2, t] = error "Qubit fuera de rango"
  | c1 == c2 || c1 == t || c2 == t = error "Los qubits de control y objetivo deben ser distintos"
  | otherwise = [[ccnotElement i j | j <- [0..limit]] | i <- [0..limit]]
  where
    limit = 2^n - 1 :: Int
    ccnotElement :: Int -> Int -> Complex Double
    ccnotElement i j =
      -- Verificamos si AMBOS bits de control están en 1
      if testBit i c1 && testBit i c2
        then if j == (i `xor` (1 `shiftL` t)) then 1 :+ 0 else 0 :+ 0
        else if i == j then 1 :+ 0 else 0 :+ 0


------------------------------------------------
-- 5. Evaluador del AST (Semántica)
------------------------------------------------

-- Función auxiliar para validar que un qubit esté dentro del rango permitido.
-- Si falla, lanza un error que corta la ejecución monádica.
checkQubit :: Int -> Int -> QState ()
checkQubit n q =
	if q >= 0 && q < n
	then return ()
	else throwQError (QubitOutOfBounds q)

------------------
-- usedQubits :: Gate -> [Int]
-- Recorre de forma recursiva la estructura de una compuerta (o sub-circuito) y 
-- devuelve una lista con todos los índices de qubits involucrados.
-- 
-- Utilidad: Esta función es fundamental para validar el combinador paralelo (Par). 
-- En computación cuántica, dos compuertas que se ejecutan al mismo tiempo NO pueden 
-- actuar sobre el mismo qubit. Se usa junto con 'intersect' para asegurar que 
-- los conjuntos de qubits sean disjuntos.
-- 
-- Argumentos:
--	- Gate: La compuerta o árbol de compuertas a analizar.
-- Devuelve: Una lista de enteros de todos los qubits usados.
usedQubits :: Gate -> [Int]
usedQubits (H q) = [q]
usedQubits (X q) = [q]
usedQubits (CNOT c t) = [c, t]
usedQubits (CCNOT c1 c2 t) = [c1, c2, t]
usedQubits (Seq g1 g2) = usedQubits g1 ++ usedQubits g2
usedQubits (Par g1 g2) = usedQubits g1 ++ usedQubits g2

------------------
-- gateToMatrix :: Int -> Gate -> QState Matrix
-- Transforma la sintaxis abstracta (AST) en su denotación matemática (matrices).
--
-- Argumentos:
-- 	- Int: número de qubits del circuito (dimensión del espacio de estados global).
-- 	- Gate: la compuerta cuántica a convertir a matriz (del AST).
--  - Retorna: un QState Matrix. Una matriz de 2^n x 2^n que representa la 
--    compuerta expandida, encapsulada en la mónada QState para manejo de errores.
--
-- Casos de evaluación:
-- a. Compuertas unarias (H, X):
--    Se valida el rango del qubit y se expande mediante producto tensorial con Identidades.
-- b. CNOT:
--    Verifica control y objetivo, y construye la matriz de permutación condicionada.
-- c. Toffoli (CCNOT):
--    Similar a CNOT pero requiere que dos bits de control sean 1 para invertir el objetivo.
-- d. Combinador Secuencial (Seq):
--    Representa la ejecución temporal (g1 luego g2). Matemáticamente es M2 * M1.
-- e. Combinador Paralelo (Par):
--    Representa la aplicación simultánea en qubits distintos. Como nuestras matrices 
--    ya están expandidas al espacio global de n qubits, g1 y g2 conmutan. Por lo tanto, 
--    se implementa como una composición (multiplicación), validando previamente que 
--    los qubits sean disjuntos mediante 'usedQubits'.
gateToMatrix :: Int -> Gate -> QState Matrix
gateToMatrix n (H q) = do
	checkQubit n q
	addTrace ("Hadamard aplicada al qubit " ++ show q)
	return (expandGate n q matH)
gateToMatrix n (X q) = do
	checkQubit n q
	addTrace ("Compuerta X (NOT) aplicada al qubit " ++ show q)
	return (expandGate n q matX)
gateToMatrix n (CNOT c t) = do
	checkQubit n c
	checkQubit n t
	addTrace ("CNOT: control="  ++ show c ++ ", objetivo=" ++ show t)
	return (expandCNOT n c t)
gateToMatrix n (CCNOT c1 c2 t) = do
	checkQubit n c1
	checkQubit n c2
	checkQubit n t
	addTrace ("CCNOT: control="  ++ show c1 ++ ", control=" ++ show c2 ++ ", objetivo=" ++ show t)
	return (expandCCNOT n c1 c2 t)
gateToMatrix n (Seq g1 g2) = do
	m1 <- gateToMatrix n g1
	m2 <- gateToMatrix n g2
	return (mmult m2 m1)
gateToMatrix n (Par g1 g2) = do
	let qs1 = usedQubits g1
	let qs2 = usedQubits g2
	if null (intersect qs1 qs2)
		then do
			m1 <- gateToMatrix n g1
			m2 <- gateToMatrix n g2
			return (mmult m2 m1)
		else throwQError (InvalidCircuit "Compuertas paralelas deben usar qubits disjuntos")

------------------
-- applyCircuit :: Int -> Gate -> QState ()
-- Ejecuta la lógica de una compuerta sobre el estado actual del sistema.
-- 
-- El proceso sigue estos pasos:
--   1. Obtiene la matriz global de la compuerta mediante 'gateToMatrix'.
--   2. Recupera el vector de estado actual (|ψ>) con 'getState'.
--   3. Aplica la transformación lineal (multiplicación matriz-vector).
--   4. Actualiza el estado global del sistema con el nuevo vector resultante.
--
-- Argumentos:
--   - n: Cantidad total de qubits en el sistema.
--   - g: Estructura de la compuerta o circuito a aplicar.
applyCircuit :: Int -> Gate -> QState ()
applyCircuit n g = do
	mat <- gateToMatrix n g
	st <- getState
	putState (mvecmult mat st)

------------------
-- Función eval principal
-- Evalúa un circuito cuántico completo transformándolo en sus operaciones y aplicándolas
-- al estado inicial del sistema.
--
-- El proceso sigue estos pasos:
--   1. Extrae el número de qubits (n) y las compuertas (gs) del circuito.
--   2. Usa gateToMatrix para convertir la estructura de compuertas a una matriz única.
--   3. Ejecuta la mónada QState con el estado inicial para obtener la matriz final y la traza.
--   4. Si hay error, lo propaga. Si es exitoso:
--		- Aplica la matriz final al estado inicial mediante mvecmult (U|ψ₀⟩).
--      - Retorna el estado final y el log de operaciones.
--
-- Argumentos:
--   - Circuit - El circuito cuántico a evaluar (contiene número de qubits y compuertas).
--
-- Retorna:
--   - Left QError: si ocurrió un error durante la ejecución
--   - Right (StateVector, QTrace): el vector de estado final y el registro de operaciones
eval :: Circuit -> Either QError (StateVector, QTrace)
eval (Circuit n gs) 
	| n > 20 = Left (TooManyQubits n)
	| otherwise = case runQState (applyCircuit n gs) (initialState n) of
		Left err -> Left err
		Right (_, finalState, traza) -> Right (finalState, traza)