#!/bin/bash

# change this with the command for your own installed SAT solver
SATSOLVER=./maplecomsps_lrb

GENERATOR=./gensat_smooth
DIR=./bench/

# do fewer runs for larger numbers
# these are somewhat arbitrary
BITS=(8 12 16 18 20 9999999)
SEEDS=(64 16 4 2 1)
NUMBERS=(32 8 4 2 1)

i=0
nbits=${BITS[$i]}
nseeds=${SEEDS[$i]}
nnumbers=${NUMBERS[$i]}

trap "exit" INT
while true; do
    # bounds for numbers with exactly n bits
    lower=$((1 << ($nbits - 1) ))
    upper=$(((1 << $nbits) - 1))
    if [ "$nbits" -eq "${BITS[$i]}" ]; then
        nseeds=${SEEDS[$i]}
        nnumbers=${NUMBERS[$i]}
        i=$(($i + 1))
    fi
    echo "n: $nbits, count: $nnumbers, seeds: $nseeds, range: [$lower, $upper]"

    # generate $nnumbers numbers with n bits
    trap "exit" INT
    for N in $(shuf -i $lower-$upper -n $nnumbers); do
        # generate the Circuit-SAT instance
        $GENERATOR $N >"${DIR}${N}.dimacs"
        printf "${N}"
        mkdir "${DIR}sol_${N}"

        # solve it $nseeds times
        trap "exit" INT
        for seed in $(seq 1 $nseeds); do
            printf "."
            start=$(date +%s.%N)
            $SATSOLVER -rnd-init -rnd-seed=$seed <"${DIR}${N}.dimacs" >"${DIR}sol_${N}/${seed}.dimacs"
            end=$(date +%s.%N)
            echo $N $seed $(echo $end - $start | bc) >> timing.txt
        done
        printf "\n"
    done
    nbits=$(($nbits+1))
done

