main: phony
	mlton -default-type int64 -default-type word64 -output main main.mlton.mlb

test: phony
	mlton -default-type int64 -default-type word64 -output test test.mlton.mlb

.PHONY: phony
phony: