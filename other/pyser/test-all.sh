#!/bin/bash
for f in ~/QASMBench/large/*; do
    echo $f
    bn=$(basename $f)
    echo $bn
    g="$f/$bn.qasm"
    echo $g
    python pyser.py $g
    s=$?
    echo "$bn $s" >> large.txt
done
