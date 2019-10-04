#!/usr/bin/env python3

import sys

def bits2uint(bits):
    n = 0
    for b in reversed(bits):
        n *= 2
        n += b
    return n

def bits2int(bits):
    return bits2uint(bits[:-1]) - (bits[-1] * 2**(len(bits)-1))

def var2uint(variables, values):
    bits = [(1 if values[abs(v)] == v else 0) for v in variables]
    return bits2uint(bits)

def var2int(variables, values):
    bits = [(1 if values[abs(v)] == v else 0) for v in variables]
    return bits2int(bits)

def read_instance(inst):
    pebits = []
    # read the instance to interpret variable meanings
    for line in inst:
        if not (line.startswith('c') or line.startswith('p')):
            continue # only interested in file header
        if line.startswith('c m = '):
            words = line.split()
            m = int(words[3])
        elif line.startswith('c a := '):
            words = line.split()
            variables = words[3].strip('[]')
            abits = [int(x) for x in variables.split(',')]
        elif line.startswith('c b := '):
            words = line.split()
            variables = words[3].strip('[]')
            bbits = [int(x) for x in variables.split(',')]
        elif line.startswith('c cs = '):
            words = line.split()
            variables = words[3].strip('[]')
            cs = [int(x) for x in variables.split(',')]
        elif line.startswith('c pp := '):
            words = line.split()
            expression = words[3].split('^')
            p = int(expression[0])
            variables = expression[1].strip('[]')
            ebits = [int(x) for x in variables.split(',')]
            pebits.append((p,ebits))
    return (abits, bbits, cs, m, pebits)

def read_solution(sol):
    assignment = [0]
    for line in sol:
        if line.startswith('s UNSATISFIABLE'):
            return None
        if line.startswith('v '):
            assignment.extend([int(x) for x in line.split()[1:]])
    return assignment

def interpret_solution(abits, bbits, pebits, assignment):
    a = var2int(abits, assignment)
    b = var2uint(bbits, assignment)
    pes = [(p, var2uint(ebits, assignment)) for (p, ebits) in pebits]
    return a, b, pes

def pes2str(pes):
    s = ''
    for (p, e) in pes:
        s += '{}^{} * '.format(p, e)
    return s[:-3]

def evalpes(pes):
    n = 1
    for (p, e) in pes:
        n *= p**e
    return n

def rat_alg(a, b, cs, m):
    d = len(cs)-1
    rat = a + b*m
    alg = 0
    for (i, c) in enumerate(cs):
        alg += c * (a**i) * ((-b)**(d-i))
    return rat * alg

def valid_solution(a, b, cs, m, pes):
    return rat_alg(a, b, cs, m) == evalpes(pes)

def main():
    if len(sys.argv) == 3:
        with open(sys.argv[1]) as inst:
            (abits, bbits, cs, m, pebits) = read_instance(inst)
        print('Instance: cs={}, m={}'.format(cs, m))
        with open(sys.argv[2]) as sol:
            assignment = read_solution(sol)
        if assignment is None:
            print('Unsatisfiable', file=sys.stderr)
            return
        (a, b, pes) = interpret_solution(abits, bbits, pebits, assignment)
        print('Found solution: ({}, {}) => {}'.format(a, b, pes2str(pes)))
        if valid_solution(a, b, cs, m, pes):
            print('Valid solution')
        else:
            print('Invalid solution')
    else: 
        print('Usage: {} instance solution'.format(sys.argv[0]), file=sys.stderr)

if __name__ == '__main__':
    main()

