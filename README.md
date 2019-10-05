# Speeding up factoring with quantum SAT solvers

This repository contains scripts complementary to the paper
[Speeding up factoring with quantum SAT solvers](#TODO: link).


# Example usage

You probably want to reproduce the results from the paper or build
do your own benchmarking. Here is how you could do that.


## Reproduce our results

Extract the circuits SAT instances and their solutions (warning: lots
of data):

```bash
$ 7z -x bench.7z
```

Simplify the SAT instances, which can reduce many variables and
clauses. This requires your solver to have this capability. I used
[MapleComSPS](https://sites.google.com/a/gsd.uwaterloo.ca/maplesat/),
but I believe all SAT solvers based on [minisat](http://minisat.se/)
have this capability. Accordingly you may have to change the command in
the `simp.py` script, then run:

```bash
$ python3 simp.py
```

Now you should be able to reproduce our results:

```bash
$ python3 measure.py
```

or to get additional statistics:

```bash
$ python3 measure.py more
```

## Generate new data

You can also generate your own circuits and run benchmarks on solving
them. The circuits are generated by a single Haskell binary. First,
compile it:

```bash
$ ghc -O gensat_smooth.hs
```

Then run it, passing the parameter N for which you want to generate
the smoothness circuit. For example when N=1337:

```bash
$ ./gensat_smooth 1337 > bench/1337.dimacs
```

Do not pick N too small or the circuit may not make sense. Optionally you could
include a second parameter that limits the size of the exponents to some
constant, which shrinks the circuit input size significantly while most smooth
F(a,b) will still be detected.

Now you can solve the instance (assuming your SAT solver command is `maplecomsps_lrb`):

```bash
$ maplecomsps_lrb < bench/1337.dimacs > bench/sol_1337/1.dimacs
```

Before you go and run more benchmarks, now is a good time to check if the found
solution makes sense. The following script should tell you which parameters a,b
were found and how to write F(a,b) as a product of small primes.

```bash
$ python3 check_solution.py bench/1337.dimacs bench/sol_1337/1.dimacs
```

Running more benchmarks can be done with the following script (this does
not terminate by itself, so you'll have to stop it by sending SIGTERM):

```bash
$ bash bench.sh
```

You probably want to adjust many parameters in the `bench.sh` script before
you run it.

If you solve the same instance multiple times, you may be interested in
seeing which smooth numbers were found.

```bash
$ python3 check_solutions.py bench/1337.dimacs bench/sol_1337/
```

