MPL_COMPILER=mpl
MLTON_COMPILER=mlton

MPL=$(MPL_COMPILER) -mlb-path-var 'COMPILER mpl'
MLTON=$(MLTON_COMPILER) -mlb-path-var 'COMPILER mlton'

FLAGS=-verbose 1 -default-type int64 -default-type word64 -cc-opt "-O3 -march=native"
FLAGS_DBG=$(FLAGS) -debug true -debug-runtime true -keep g

main.mpl: phony
	$(MPL) $(FLAGS) -output main.mpl src/main.mlb

main.mpl.dbg: phony
	$(MPL) $(FLAGS_DBG) -output main.mpl.dbg src/main.mlb

main.mlton: phony
	$(MLTON) $(FLAGS) -output main.mlton src/main.mlb

.PHONY: phony
phony:
