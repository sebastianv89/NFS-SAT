#!/usr/bin/env python3

import subprocess
from pathlib import Path

def simplify(bench_dir, solver):

    for fname in Path(bench_dir).glob('*.dimacs'):
        if fname.name.startswith('simp_'):
            continue
        output = Path(bench_dir, 'simp_{}.dimacs'.format(fname.stem))
        if output.exists():
            continue
        cmd = '{} -dimacs={} {}'.format(solver, output, fname).split()
        subprocess.run(cmd)

def main():
    simplify('./bench', './maplecomsps_lrb')

if __name__ == '__main__':
    main()
