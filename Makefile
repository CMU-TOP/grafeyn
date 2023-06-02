# MPL_COMPILER=mpl
# MPL_COMPILER=/home/ec2-user/proj/mpl/em/build/bin/mpl
MPL_COMPILER?=/usr0/home/swestric/proj/mpl/em/build/bin/mpl
# MPL_COMPILER=/usr0/home/swestric/proj/mpl/sam-heartbeat/build/bin/mpl -mlb-path-var 'PICK_FJ greedy-work-amortized'

MPL=$(MPL_COMPILER) -disable-pass splitTypes1 -disable-pass splitTypes2

FLAGS=-default-type int64 -default-type word64

main.mpl: phony
	$(MPL) $(FLAGS) -mlb-path-var 'COMPILER mpl' -output main.mpl main.mlb

main.mlton: phony
	mlton $(FLAGS) -mlb-path-var 'COMPILER mlton' -output main.mlton main.mlb

all-main.mpl: phony
	$(MPL) $(FLAGS) -mlb-path-var 'COMPILER mpl' -output all-main.mpl all-main.mlb

all-main.mlton: phony
	mlton $(FLAGS) -mlb-path-var 'COMPILER mlton' -output all-main.mlton all-main.mlb

test: phony
	mlton $(FLAGS) -output test test.mlton.mlb

.PHONY: phony
phony:
