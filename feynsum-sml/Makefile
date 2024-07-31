MPL_COMPILER=mpl
MPL=$(MPL_COMPILER)
FLAGS=-verbose 1 -default-type int64 -default-type word64 -cc-opt "-O3 -march=native"

main.mpl: phony
	$(MPL) $(FLAGS) -mlb-path-var 'COMPILER mpl' -output main.mpl src/main.mlb

main.mlton: phony
	mlton $(FLAGS) -mlb-path-var 'COMPILER mlton' -output main.mlton src/main.mlb

.PHONY: phony
phony:
