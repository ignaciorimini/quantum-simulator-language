module Parser where

{- 
Este módulo se encarga de transformar el código fuente (texto) en el AST (Circuito).
Utiliza la librería Parsec para definir una gramática formal para nuestro lenguaje cuántico.

GRAMÁTICA DEL LENGUAJE:
<circuit>     ::= 'qubits' <number> <gateExpr>
<gateExpr>    ::= <gateTerm>   { ';'  <gateTerm>   }  (Secuencial: g1 ; g2)
<gateTerm>    ::= <gateFactor> { '||' <gateFactor> }  (Paralelo:   g1 || g2)
<gateFactor>  ::= 'H' <number> 
                | 'X' <number> 
                | 'CNOT' <number> <number>
                | 'CCNOT' <number> <number> <number>
                | '(' <gateExpr> ')'
-}

import AST
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)

------------------------------------------------
-- 1. LEXER (Analizador Léxico)
------------------------------------------------

-- Define los tokens (palabras clave, operadores, comentarios) básicos del lenguaje.
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    Token.commentLine     = "//",
    Token.reservedNames   = ["qubits", "H", "X", "CNOT", "CCNOT"],
    Token.reservedOpNames = [";", "||"]
}

-- Definición de utilidades para consumir tokens sin preocuparse por espacios en blanco.
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
integer    = Token.integer lexer

--Parsea un número entero positivo para ser usado como índice de qubit.
natural :: Parser Int
natural = fromInteger <$> integer


------------------------------------------------
-- 2. PARSER (Analizador Sintáctico)
------------------------------------------------

-- gateExpr: Maneja la composición secuencial (el operador ';').
-- Tiene la menor precedencia, por lo que agrupa términos separados por ';'.
-- Ejemplo: "g1 || g2 ; g3" se parsea como "(g1 || g2) ; g3".
gateExpr :: Parser Gate
gateExpr = chainl1 gateTerm (reservedOp ";" >> return Seq)

-- gateTerm: Maneja la composición en paralelo (el operador '||').
-- Tiene mayor precedencia que ';'.
gateTerm :: Parser Gate
gateTerm = chainl1 gateFactor (reservedOp "||" >> return Par)

-- gateFactor: Los "átomos" de la lógica: compuertas básicas o expresiones entre paréntesis.
gateFactor :: Parser Gate
gateFactor = parens gateExpr
            <|> (reserved "H"  >> H  <$> natural)
            <|> (reserved "X"  >> X  <$> natural)
            <|> (reserved "CNOT" >> CNOT <$> natural <*> natural)
            <|> (reserved "CCNOT" >> CCNOT <$> natural <*> natural <*> natural)


------------------------------------------------
-- 3. PUNTO DE ENTRADA
------------------------------------------------

-- circuitParser: Parser principal del archivo completo.
-- Estructura: "qubits <n> <computación>"
circuitParser :: Parser Circuit
circuitParser = do
    whiteSpace          -- Ignora espacios y comentarios iniciales.
    reserved "qubits"   -- Obligación de empezar con la definición de qubits.
    n <- natural        -- Tamaño del sistema.
    g <- gateExpr       -- Árbol de compuertas.
    eof                 -- Error si sobra texto al final del archivo.
    return (Circuit n g)

-- parseCircuit: Interfaz pública para ejecutar el parser sobre un String.
-- 
-- Argumentos:
--  - filename: Nombre del archivo para reportar errores en la consola.
--  - input: El contenido textual del archivo .q a procesar.
--  - Retorna: Un Either con el error detallado o el circuito parseado.
parseCircuit :: String -> String -> Either ParseError Circuit
parseCircuit filename input = parse circuitParser filename input