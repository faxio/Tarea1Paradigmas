SRC=main.hs
all:
	stack exec -- ghc -dynamic -threaded -eventlog -rtsopts ${SRC} -o prog
	#   -dynamic  : para algunas distribuciones (ej: Arch Linux)
	#   -threaded : permitir el uso de threads
	#   -eventlog : registrar los eventos en un log para futuro uso (threadscope)
	#   -rtsopts  : habilitar opciones RTS en tiempo de ejecucion
	#
	#   Al ejecutar:
	#   ./prog args +RTS -NX -ls -RTS
	#   NX -> N1, N2, .... numero de CPU threads en un pool.
