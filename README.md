# QuantumSim: Simulador de Circuitos Cuánticos en Haskell

Un simulador de computación cuántica basado en un **EDSL** (Embedded Domain-Specific Language) desarrollado para la materia **Análisis de Lenguajes de Programación (ALP)** de la Licenciatura en Ciencias de la Computación (FCEIA - UNR).

## 🚀 Características
- **Sintaxis Propia**: Interfaz textual mediante archivos `.q` para definir circuitos de forma imperativa.
- **Deep Embedding**: El AST permite múltiples interpretaciones y validaciones estáticas.
- **Mónada QState**: Motor de ejecución que combina Estado, Errores y Trazas de forma puramente funcional.
- **Álgebra Lineal Eficiente**: Implementación de compuertas controladas (CNOT, CCNOT) mediante manipulación de bits, evitando la sobrecarga de SWAPs.
- **Notación de Dirac**: Salida formateada de forma legible con soporte para amplitudes complejas.
- **Algoritmos Incluidos**: Estado de Bell, GHZ, Algoritmo de Grover y Medio Sumador (Half-Adder).

## 🛠️ Instalación y Uso
Este proyecto utiliza [Stack](https://docs.haskellstack.org/).

### 1. Compilar el proyecto
```bash
stack build
```

### 2. Ejecutar ejemplos predefinidos
```bash
stack run
```

### 3. Simular un archivo específico
```bash
stack run examples-q/bell.q
```

## 📂 Estructura del Proyecto
- `app/Main.hs`: Punto de entrada del ejecutable.
- `src/AST.hs`: Definición del árbol de sintaxis abstracta mediante GADTs.
- `src/Parser.hs`: Analizador sintáctico monádico utilizando Parsec con jerarquía de precedencia.
- `src/Eval.hs`: Semántica del lenguaje, álgebra lineal y lógica de la mónada `QState`.
- `src/PrettyPrinter.hs`: Formateador para la representación de estados en notación de Dirac.
- `examples-q/`: Colección de circuitos de prueba y algoritmos cuánticos.

## 🎓 Créditos
Desarrollado por **Ignacio Rímini** para la cátedra de Análisis de Lenguajes de Programación.
Marzo 2026.

