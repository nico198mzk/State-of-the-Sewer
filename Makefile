# Makefile para State-of-the-Sewer (Haskell/Gloss)

.PHONY: all run clean

# Regla por defecto: compilar el proyecto
all:
	stack build

# Ejecutar el juego
run:
	stack run

# Limpiar archivos generados
clean:
	stack clean
