#!/usr/bin/env python3

import sys
import check_solution as check
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict
import os.path
import pickle

DIR = 'bench'
TIMING_FILE = 'timing.txt'

def parse_timing(fname):
    timing = defaultdict(dict)
    with open(fname) as f:
        for line in f:
            if line.startswith('#'):
                continue
            [n, seed, time] = line.split()
            timing[int(n)][int(seed)] = float(time)
    return timing

def count_vars(fname):
    e = 0
    with open(fname) as f:
        for line in f:
            if not (line.startswith('c') or line.startswith('p')):
                break # only parse leading comment
            if line.startswith('c pp := '):
                words = line.split()
                es = words[3].split('^')[1].strip('[]').split(',')
                e = max(e, int(es[-1]))
    return e

def count_simp(fname):
    with open(fname) as f:
        return int(f.readline().split()[2])

def collect_data():
    '''
    return data in format
    data = {
        N(int): {
            'size': int,                    // log_2(N)
            'circuit_vars': int,            // # circuit input bits
            'sat_vars': int,                // # SAT variables, after simplification
            'satisfiable': bool,
            'solutions': {
                seed(int): {
                    'time': float,          // solve time in seconds
                    // sol, Fab only if satisfiable
                    'sol': (                // founds solution to the smoothness eqn
                        a(int),
                        b(int),
                        [(p(int), e(int))]
                    ),
                    'Fab': int,
                }
            }
        }
    }
    '''

    # collect timing
    timing = parse_timing('{}'.format(TIMING_FILE))

    # collect rest of data
    input_vars = {}
    simp_vars = {}
    solutions = defaultdict(lambda: defaultdict(dict))
    for n in timing:
        inst_fname = '{}/{}.dimacs'.format(DIR, n)
        input_vars[n] = count_vars(inst_fname)
        simp_vars[n] = count_simp('{}/simp_{}.dimacs'.format(DIR, n))
        with open(inst_fname) as fi:
            (ab, bb, cs, m, pebs) = check.read_instance(fi)
        for seed in timing[n]:
            sol_fname = '{}/sol_{}/{}.dimacs'.format(DIR, n, seed)
            with open(sol_fname) as fs:
                asgn = check.read_solution(fs)
            if asgn is None:
                solutions[n] = None
                print('Unsatisfiable {}'.format(n), file=sys.stderr)
                break 
            (a, b, pes) = check.interpret_solution(ab, bb, pebs, asgn)
            if not check.valid_solution(a, b, cs, m, pes):
                print('Invalid solution {} {}'.format(n, seed), file=sys.stderr)
                continue
            solutions[n][seed]['sol'] = (a, b, pes)
            solutions[n][seed]['Fab'] = check.evalpes(pes)

    # merge data
    data = defaultdict(dict)
    for n in timing:
        data[n]['size'] = n.bit_length()
        data[n]['circuit_vars'] = input_vars[n]
        data[n]['sat_vars'] = simp_vars[n]
        if solutions[n] is not None:
            data[n]['satisfiable'] = True
            data[n]['solutions'] = solutions[n]
        else:
            data[n]['satisfiable'] = False
            data[n]['solutions'] = defaultdict(dict)
        for (seed, time) in timing[n].items():
            data[n]['solutions'][seed]['time'] = time
    return data

def L(n):
    return np.exp(np.power(np.log(n), 1/3)*np.power(np.log(np.log(n)), 2/3))

def y(n):
    return np.power(L(n), np.power(1/3, 1/3))

def visualize(data):
    # plot sizeof(N) against runtime
    xs_sat, xs_unsat = [], []
    x_line = np.logspace(np.log(min(data.keys())), np.log(max(data.keys())), num=200, base=np.e)
    y_line1 = list(map(lambda x: np.power(L(x), 1.387), list(x_line)))
    y_line2 = list(map(lambda x: np.power(L(x), 1.923), list(x_line)))
    y_line3 = list(map(lambda x: np.power(2, y(x)), list(x_line)))
    ys_sat, ys_unsat = [], []
    for n in data:
        yss = []
        for solution in data[n]['solutions'].values():
            yss.append(solution['time'])
        if data[n]['satisfiable']:
            xs_sat.append(n)
            ys_sat.append(y(n) * np.median(yss))
        else:
            xs_unsat.append(n)
            ys_unsat.append(y(n) * np.median(yss))
    min_size = min(data[n]['size'] for n in data) - 1
    max_size = max(data[n]['size'] for n in data) + 1
    plt.xscale('log')
    plt.yscale('log')
    plt.scatter(xs_sat, ys_sat, label='y(N) * time to solve one instance (satisfiable)')
    plt.scatter(xs_unsat, ys_unsat, label='y(N) * time to solve one instance (unsatisfiable)', color='red')
    l3 = plt.plot(x_line, y_line3, label='$2^{y}$')
    l1 = plt.plot(x_line, y_line1, label='$L^{1.387}$')
    l2 = plt.plot(x_line, y_line2, label='$L^{1.923}$')
    plt.xticks([2**i for i in range(min_size, max_size)],
            ['$2^{{{}}}$'.format(i) for i in range(min_size, max_size)])
    plt.tick_params(axis='x', which='minor', bottom=False)
    plt.xlabel('N')
    plt.ylabel('solver runtime (s)')
    plt.legend()
    plt.title('Runtime of solving the variable exponent circuit')

    plt.show()

def visualize_more(data):
    plt.figure(1)

    # plot sizeof(N) against #sat_variables
    xs = list(data.keys())
    ys = [data[n]['sat_vars'] for n in data]
    plt.subplot(2, 2, 1)
    plt.xscale('log')
    plt.xlabel('N')
    plt.ylabel('size of circuit (# SAT variables)')
    plt.scatter(xs, ys)

    # plot sizeof(N) against runtime
    xs_small, xs_large = [], []
    ys_small, ys_large = [], []
    for n in data:
        if data[n]['size'] < 18:
            xs_small.append(n)
            ys_small.append([])
            for solution in data[n]['solutions'].values():
                ys_small[-1].append(solution['time'])
        else:
            # we did only a few measurements for >=18-bit numbers, so just show them all
            for solution in data[n]['solutions'].values():
                xs_large.append(n)
                ys_large.append(solution['time'])
    min_size = min(data[n]['size'] for n in data) - 1
    max_size = max(data[n]['size'] for n in data) + 1
    plt.subplot(2, 2, 2)
    plt.xscale('log')
    plt.yscale('log')
    plt.boxplot(ys_small, positions=xs_small)
    plt.scatter(xs_large, ys_large)
    plt.xticks([2**i for i in range(min_size, max_size)],
            ['$2^{{{}}}$'.format(i) for i in range(min_size, max_size)])
    plt.xlabel('N')
    plt.ylabel('solver runtime (s)')

    # plot |F(a,b)| against runtime
    xs, ys = [], []
    for n in data:
        if not data[n]['satisfiable']:
            continue
        for solution in data[n]['solutions'].values():
            xs.append(abs(solution['Fab']))
            ys.append(solution['time'])
    min_size = min(xs).bit_length() - 1
    max_size = max(xs).bit_length() + 1
    plt.subplot(2, 2, 3)
    plt.xscale('log')
    plt.yscale('log')
    plt.xticks([2**i for i in range(min_size, max_size, 2)],
            ['$2^{{{}}}$'.format(i) for i in range(min_size, max_size, 2)])
    plt.xlabel('|F(a,b)|')
    plt.ylabel('solver runtime (s)')
    plt.scatter(xs, ys)

    # plot #circuit_variables against runtime
    xs, ys = [], []
    for n in data:
        xs.append(data[n]['circuit_vars'])
        yss = []
        for solution in data[n]['solutions'].values():
            yss.append(solution['time'])
        ys.append(np.mean(yss))
    plt.subplot(2, 2, 4)
    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('circuit input variables')
    plt.ylabel('solver runtime (s)')
    plt.scatter(xs, ys)

    plt.show()

def main():
    pfname = 'measure.pickle'
    if os.path.isfile(pfname) and os.path.getmtime(pfname) >= os.path.getmtime(TIMING_FILE):
        with open(pfname, 'rb') as pf:
            data = pickle.load(pf)
    else:
        data = collect_data()
        with open(pfname, 'wb') as pf:
            pickle.dump(data, pf)

    if len(sys.argv) > 1:
        if sys.argv[1] == 'more':
            visualize_more(data)
        else:
            print('Usage: {} [more]'.format(sys.argv[0]))
    else:
        visualize(data)

if __name__ == '__main__':
    main()
