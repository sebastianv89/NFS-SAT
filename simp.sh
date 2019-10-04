#!/bin/bash

cd ./bench/
for f in *.dimacs ; do
    ../maplecomsps_lrb -dimacs=simp_$f $f
done

