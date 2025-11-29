# 🐀 Haski-RPG: State of the Sewer

> **Videojuego RPG de acción tipo Dungeon Crawler desarrollado en Haskell usando la librería Gloss**

Un juego roguelike donde debes explorar las alcantarillas, enfrentarte a criaturas hostiles y escapar atravesando **3 pisos** proceduralmente generados.

---

## Integrantes

| Nombre              | Rol                  |
| ------------------- | -------------------- |
| [Nombre Integrante] | [Rol en el proyecto] |
| [Nombre Integrante] | [Rol en el proyecto] |
| [Nombre Integrante] | [Rol en el proyecto] |

---

## Guía de Instalación (Tutorial Multiplataforma)

### Requisitos Generales

- **Stack**: Gestor de proyectos Haskell
- **Librerías gráficas**: FreeGLUT y OpenGL (requeridas por Gloss)

---

### MacOS

Instalar usando Homebrew:

```bash
# Instalar Homebrew si no lo tienes
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Instalar Stack y dependencias gráficas
brew install haskell-stack freeglut

# Verificar instalación
stack --version
```

---

### Linux (Ubuntu/Debian)

Instalar usando APT:

```bash
# Actualizar repositorios
sudo apt update

# Instalar Stack
sudo apt install haskell-stack

# Instalar dependencias gráficas para Gloss
sudo apt install freeglut3-dev libglu1-mesa-dev mesa-common-dev

# Actualizar Stack a la última versión
stack upgrade

# Verificar instalación
stack --version
```

---

### Linux (Arch/CachyOS/Manjaro)

Instalar usando Pacman:

```bash
# Instalar Stack y dependencias gráficas
sudo pacman -S stack freeglut glu mesa

# Verificar instalación
stack --version
```

---

### Windows

1. **Descargar Stack**: Ir a [https://docs.haskellstack.org/](https://docs.haskellstack.org/) y descargar el instalador de Windows.

2. **Ejecutar el instalador** y seguir las instrucciones.

3. **Librerías gráficas**: Generalmente vienen incluidas con los drivers de GPU. Si hay problemas, instalar [freeglut para Windows](https://www.transmissionzero.co.uk/software/freeglut-devel/).

4. **Verificar instalación**:
   ```powershell
   stack --version
   ```

---

## Cómo Compilar y Jugar

### Usando el Makefile (Recomendado)

```bash
# Compilar el proyecto
make all

# Ejecutar el juego
make run

# Limpiar archivos generados
make clean
```

### Usando Stack directamente

```bash
# Compilar el proyecto
stack build

# Ejecutar el juego
stack run

# Limpiar archivos generados
stack clean
```

---

## Controles

| Tecla       | Acción                       |
| ----------- | ---------------------------- |
| **W**       | Moverse hacia arriba         |
| **A**       | Moverse hacia la izquierda   |
| **S**       | Moverse hacia abajo          |
| **D**       | Moverse hacia la derecha     |
| **Espacio** | Atacar (combate direccional) |
| **Esc**     | Salir del juego              |

> **Nota:** El combate es direccional - el jugador ataca en la dirección hacia la que está mirando (indicada por el triángulo amarillo).

---

## Informe Técnico: Uso de la Mónada State (Requisito Académico)

### Gestión del Estado con la Mónada State

El proyecto utiliza la **Mónada State** del paquete `mtl` para gestionar el `GameState` del juego. Esta abstracción permite encapsular el estado mutable dentro de un contexto puramente funcional, evitando el paso explícito del estado entre funciones. En lugar del enfoque tradicional donde cada función recibe y retorna el `GameState` (ej: `updatePlayer :: Float -> GameState -> GameState`), utilizamos el tipo `State GameState ()` que representa una computación con estado implícito. Esto permite componer múltiples transformaciones de estado usando la notación `do`, donde las funciones `get`, `put` y `modify` abstraen el acceso al estado sin necesidad de pasarlo manualmente entre cada llamada.

### Contraste: Método Antiguo vs Método Nuevo

| Aspecto              | Paso Explícito (Antiguo)                                        | Mónada State (Nuevo)                                               |
| -------------------- | --------------------------------------------------------------- | ------------------------------------------------------------------ |
| **Firma de función** | `updatePlayer :: Float -> GameState -> GameState`               | `movePlayerByKeys :: Float -> State GameState ()`                  |
| **Encadenamiento**   | `checkCollisions (updateEnemies dt (updatePlayer dt oldState))` | `do { movePlayerByKeys dt; updateEnemies dt; cleanupDeadEnemies }` |
| **Acceso al estado** | Parámetro explícito en cada función                             | `get` para leer, `put`/`modify` para escribir                      |
| **Ejecución**        | Aplicación directa de funciones                                 | `execState (updateWorldM dt) gs`                                   |

### Ejemplo de Implementación

```haskell
-- Función principal compatible con Gloss
updateWorld :: Float -> GameState -> GameState
updateWorld dt gs = execState (updateWorldM dt) gs

-- Lógica interna usando la mónada State
updateWorldM :: Float -> State GameState ()
updateWorldM dt = do
  gs <- get                    -- Obtener estado actual
  case gsPhase gs of
    Playing -> do
      movePlayerByKeys dt      -- Mover jugador
      updateEnemies dt         -- Actualizar enemigos
      enemyDealDamage dt       -- Aplicar daño
      cleanupDeadEnemies       -- Limpiar enemigos muertos
      checkStairTransition     -- Verificar transición de piso
    _ -> return ()
```

### Beneficios Obtenidos

1. **Composición limpia**: Las funciones se encadenan con `do`-notation sin pasar el estado manualmente.
2. **Código más legible**: La secuencia de operaciones es clara y declarativa.
3. **Menos errores**: No hay riesgo de usar una versión desactualizada del estado.
4. **Compatibilidad con Gloss**: `execState` convierte `State GameState ()` a la firma `GameState -> GameState` que Gloss espera.

---

## Mecánicas Implementadas

### 1. Generación Procedural de Salas y Pasillos

El módulo `WorldGen.hs` implementa un **algoritmo de Salas y Pasillos** para crear mazmorras únicas en cada partida:

- **Generación de Salas**: Se crean 15 habitaciones rectangulares (6-12 tiles) sin superposición.
- **Conexión con Pasillos**: Pasillos de 2 tiles de ancho conectan salas consecutivas usando un patrón en L.
- **Auto-Tiling**: Los tiles vacíos (`Void`) adyacentes a suelo se convierten automáticamente en muros.
- **Variación Visual**: 4 variantes de textura por tile con distribución ponderada para mayor variedad.

### 2. Combate Direccional con Knockback

El sistema de combate incluye:

- **Ataque Direccional**: El jugador ataca en la dirección que mira (`DirUp`, `DirDown`, `DirLeft`, `DirRight`).
- **Knockback**: Los enemigos son empujados al recibir daño (si no hay pared detrás).
- **Rango de Ataque**: 40 píxeles desde la posición del jugador.
- **Animación de Espada**: Visible durante 0.3 segundos al atacar.

### 3. Sistema de Pisos (Meta: Completar 3 Pisos)

La progresión del juego consiste en:

- **3 Pisos Únicos**: Cada piso tiene tiles visuales distintos para diferenciarse.
- **Boss por Piso**: Al eliminar todos los enemigos normales, aparece un jefe.
- **Escalera**: Solo aparece tras derrotar al boss del piso.
- **Condición de Victoria**: Completar los 3 pisos muestra la pantalla "ESCAPED! - FLOORS CLEARED: 3/3".

### 4. Enemigos (Slimes) y Boss

| Tipo      | HP    | ATK  | Comportamiento                           |
| --------- | ----- | ---- | ---------------------------------------- |
| **Slime** | 30-60 | 8-15 | Persecución cuando el jugador está cerca |
| **Boss**  | 300   | 20   | Más rápido, aparece tras limpiar la sala |

Los enemigos usan IA de persecución básica: se mueven hacia el jugador cuando está a menos de 400 píxeles de distancia.

### 5. Sistema de Items (Drops)

Al derrotar enemigos hay 30% de probabilidad de obtener:

| Item            | Efecto           |
| --------------- | ---------------- |
| **Comida**      | +20 HP           |
| **Boost ATK**   | +5 Puntos de ATK |
| **Boost Speed** | +10 de Velocidad |

### 6. Assets Personalizados

- Sprites de jugador, enemigos (slimes) y boss
- Tiles de suelo con múltiples variantes (3 capas para los 3 pisos)
- Tiles de muros con variaciones
- Sprites de items y espada
- Pantallas de menú, lore y victoria

---

## Estructura del Proyecto

```
State-of-the-Sewer/
├── src/
│   ├── Main.hs          # Punto de entrada, configuración Gloss
│   ├── Types.hs         # Definiciones de tipos (GameState, Player, Enemy...)
│   ├── GameState.hs     # Inicialización y reset del estado
│   ├── Update.hs        # Lógica de actualización (mónada State)
│   ├── Render.hs        # Renderizado con Gloss
│   ├── Input.hs         # Manejo de teclado
│   ├── Combat.hs        # Sistema de combate y knockback
│   ├── Inventory.hs     # Sistema de items
│   ├── WorldGen.hs      # Generación procedural de mapas
│   └── Assets.hs        # Carga de imágenes PNG
├── assets/              # Sprites y texturas
├── haski-rpg.cabal      # Configuración del proyecto
├── stack.yaml           # Configuración de Stack
├── Makefile             # Comandos de compilación
└── README.md            # Este archivo
```

---

## Librerías Utilizadas

| Librería        | Uso                                     |
| --------------- | --------------------------------------- |
| **Gloss**       | Motor gráfico 2D y game loop            |
| **JuicyPixels** | Carga de imágenes PNG con transparencia |
| **random**      | Generación procedural (StdGen)          |
| **mtl**         | Mónada State para manejo de estado      |
| **containers**  | Estructuras de datos auxiliares         |

---

_Desarrollado como proyecto académico para INFO188 - Universidad Austral de Chile, 2025_
