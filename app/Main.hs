module Main where

{- 
Este archivo es el punto de entrada principal (MAIN) del simulador.
Coordina la interacción entre el usuario, el parser, el evaluador y la 
visualización final por consola.
-}

import System.Environment (getArgs)
import AST
import Eval
import PrettyPrinter
import Examples
import Parser (parseCircuit)


-- 1. EJECUCIÓN DEL CIRCUITO
-- runCircuit: Recibe un nombre y un Circuit para procesarlo.
-- Centraliza la lógica de evaluación y el reporte de errores.
runCircuit :: String -> Circuit -> IO ()
runCircuit name c@(Circuit n _) = do
    putStrLn ("==== " ++ name ++ " ====")
    putStrLn ("Circuito : " ++ ppCircuit c)

    case eval c of
        Left err -> putStrLn ("Error: " ++ show err)
        
        -- Si la ejecución fue exitosa, imprimimos la traza y el estado final.
        Right (estado, traza) -> do
            putStrLn "Pasos ejecutados:"
            mapM_ (putStrLn . ("  - " ++)) traza
            -- Usamos PrettyPrinter para convertir el vector a notación bra-ket.
            putStrLn ("Estado final: " ++ prettyPrint n estado)

    putStrLn ""


-- 2. RUTINA PRINCIPAL
main :: IO ()
main = do
    args <- getArgs
    case args of
        -- CASO A: Sin argumentos. Ejecuta los ejemplos internos de Examples.hs.
        [] -> do
            putStrLn "---- EJECUTANDO EJEMPLOS INTERNOS ----\n"
            runCircuit "Estado de Bell"                 bellState
            runCircuit "Estado GHZ"                    ghzState
            runCircuit "Compuertas en Paralelo"        ejemploParalelo
            runCircuit "Superposición Total (2 qubits)" superposicionTotal
            runCircuit "Estado con Fase Negativa"      estadoMenos
            
        -- CASO B: Un argumento (la ruta de un archivo .q).
        -- 1. Leemos el contenido del archivo de texto.
        -- 2. Intentamos parsear el contenido (usando el módulo Parser.hs).
        -- 3. Si hay error de sintaxis Parsec reportará línea y columna.
        -- 4. Si hay éxito en el parseo, ejecutamos el circuito resultante.
        [file] -> do
            content <- readFile file
            case parseCircuit file content of
                Left  err     -> putStrLn ("Error de sintaxis:\n" ++ show err)
                Right circuit -> runCircuit file circuit
                
        -- CASO C: Demasiados argumentos. Mostramos ayuda de uso.
        _ -> putStrLn "Uso: stack run [archivo.q]"
