#!/usr/bin/env python3

from collections import Counter
import os
import os.path
import sys
import check_solution as check

def main():
    if len(sys.argv) != 3:
        print('Usage: {} instance.dimacs solution_dir'.format(sys.argv[0]), file=sys.stderr)
        return
    with open(sys.argv[1]) as inst:
        (abits, bbits, cs, m, pebits) = check.read_instance(inst)
    print('a:{} bits, b:{} bits; cs={}, m={}, ps={}'.format(len(abits), len(bbits), cs, m, [p for (p,_) in pebits]))
    solutions = Counter()
    for f in os.listdir(sys.argv[2]):
        fname = os.path.join(sys.argv[2], f)
        if not os.path.isfile(fname):
            continue
        with open(fname) as sol:
            asgn = check.read_solution(sol)
        if asgn is None:
            print('Unsatisfiable', file=sys.stderr)
            continue
        (a, b, pes) = check.interpret_solution(abits, bbits, pebits, asgn)
        if not check.valid_solution(a, b, cs, m, pes):
            print("Invalid solution ({}) {}".format(fname, (a, b, pes)), file=sys.stderr)
        solutions[(a, b)] += 1
    print(solutions)

if __name__ == '__main__':
    main()

