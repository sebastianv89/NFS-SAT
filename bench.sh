#!/bin/bash

# change this with the command for your own installed SAT solver
SATSOLVER=./maplecomsps_lrb

GENERATOR=./gensat_smooth
DIR=./bench/

# do fewer runs for larger numbers
# these are somewhat arbitrary
BITS=(8 11 18 20 9999999)
SEEDS=(64 8 4 1)
NUMBERS=(32 8 4 1)

i=3
nbits=${BITS[$i]}
nseeds=${SEEDS[$i]}
nnumbers=${NUMBERS[$i]}

trap "exit" INT
while true; do
    # bounds for numbers with exactly n bits
    lower=$((1 << ($nbits - 1) ))
    upper=$((1 << $nbits))
    if [ "$nbits" -eq "${BITS[$i]}" ]; then
        nseeds=${SEEDS[$i]}
        nnumbers=${NUMBERS[$i]}
        i=$(($i + 1))
    fi
    echo $nbits, $nseeds, $nnumbers: $lower -- $upper

    # generate $nnumbers numbers with n bits
    trap "exit" INT
    for N in $(shuf -i $lower-$upper -n $nnumbers); do
        # generate the Circuit-SAT instance
        $GENERATOR $N >"${DIR}${N}.dimacs"
        echo $N generated
        mkdir "${DIR}sol_${N}"

        # solve it $nseeds times
        trap "exit" INT
        for seed in $(seq 1 $nseeds); do
            start=$(date +%s.%N)
            $SATSOLVER -rnd-init -rnd-seed=$seed <"${DIR}${N}.dimacs" >"${DIR}sol_${N}/${seed}.dimacs"
            end=$(date +%s.%N)
            echo $N $seed $(echo $end - $start | bc) >> timing.txt
        done
    done
    nbits=$(($nbits+1))
done

