main.mpl: phony
	mpl -default-type int64 -default-type word64 -output main.mpl main.mpl.mlb

main.mlton: phony
	mlton -default-type int64 -default-type word64 -output main.mlton main.mlton.mlb

test: phony
	mlton -default-type int64 -default-type word64 -output test test.mlton.mlb

.PHONY: phony
phony:
