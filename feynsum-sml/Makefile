MPL_COMPILER=mpl
# MPL_COMPILER=/home/ec2-user/proj/mpl/em/build/bin/mpl
# MPL_COMPILER?=/usr0/home/swestric/proj/mpl/em/build/bin/mpl
# MPL_COMPILER=/usr0/home/swestric/proj/mpl/sam-heartbeat/build/bin/mpl -mlb-path-var 'PICK_FJ greedy-work-amortized'

MPL=$(MPL_COMPILER) # -disable-pass splitTypes1 -disable-pass splitTypes2

FLAGS=-verbose 1 -default-type int64 -default-type word64 -cc-opt "-O3 -march=native"

main.mpl: phony
	$(MPL) $(FLAGS) -mlb-path-var 'COMPILER mpl' -output main.mpl src/main.mlb

main.mlton: phony
	mlton $(FLAGS) -mlb-path-var 'COMPILER mlton' -output main.mlton src/main.mlb

.PHONY: phony
phony:
