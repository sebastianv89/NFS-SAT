#!/usr/bin/env python3

import os
import random
from pathlib import Path
import subprocess
import time


SOLVER = './maplecomsps_lrb'
GENERATOR = './gensat_smooth'
OUTPUT_DIR = './bench/'
TIMING = './timing.txt'

# (log2 N, #N, #seeds)
INPUT = [
    [8, 32, 64],
    [12, 16, 16],
    [16, 4, 4],
    [18, 2, 2],
    [20, 1, 1]
]

def bench(solver, gen, out_dir, timing):
    [size, count, seeds] = INPUT[0]
    i = 1
    while True:
        lower = 1 << (size - 1)
        upper = 1 << size
        print(f'n: {size}, count: {count}, seeds: {seeds}, range: [{lower}, {upper})')
        for n in random.sample(range(lower, upper), count):
            instance = Path(OUTPUT_DIR, f'{n}.dimacs')
            if not instance.exists():
                with open(instance, 'w') as f:
                    subprocess.run([GENERATOR, str(n)], stdout=f)
            sol_dir = Path(OUTPUT_DIR, f'sol_{n}')
            if not sol_dir.exists():
                sol_dir.mkdir()
            print(f'{n} ', end='')
            for seed in range(1, seeds+1):
                solution = Path(sol_dir, f'{seed}.dimacs')
                if solution.exists():
                    print('x', end='')
                    continue
                print('.', end='')
                cmd = f'{SOLVER} -rnd-init -rnd-seed={seed}'.split()
                with open(instance) as fin, open(solution, 'w') as fout:
                    t0 = time.perf_counter()
                    subprocess.run(cmd, stdin=fin, stdout=fout)
                    t1 = time.perf_counter()
                with open(Path(timing), 'a') as t:
                    t.write(f'{n} {seed} {t1 - t0}\n')
            print()
        size += 1
        if i < len(INPUT) and size == INPUT[i]:
            [_, count, seeds] = INPUT[i]
            i += 1

def main():
    bench(SOLVER, GENERATOR, OUTPUT_DIR, TIMING)

if __name__ == '__main__':
    main()

