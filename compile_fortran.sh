#!/bin/bash

# This compiles the fortran code in a way that can be accessed by JNA
# It is important that FOLDER is in your java path
# the "lib" prefix in SO_PATH and the OS name are assumed by JNA

# TIP: If Permission is denied, run chmod -x on this file

OS="linux-x86-64"
FORTRAN_EXTENSION="f90"

GIT_ROOT=$(git rev-parse --show-toplevel)
FOLDER="${GIT_ROOT}/src/${OS}"
FILES="${FOLDER}/*.${FORTRAN_EXTENSION}"

for f in $FILES; do
    test -f "$f" || continue # $f becomes $FILES if there are no actual matching files

    FILE_NAME=$(basename $f)
    NAME="${FILE_NAME%.*}"

    O_PATH=$FOLDER/$NAME.o
    SO_PATH=$FOLDER/lib$NAME.so

    rm $O_PATH # todo, these should be silent if nothing is there
    rm $SO_PATH

    gfortran -fno-underscoring -fPIC -c -g -o $O_PATH $f
    gfortran -shared -o $SO_PATH $O_PATH
done

# TODO: add this as lein task or build task or something (maybe on every save or something to avoid following problem)
# TODO: compile files in the right order, or all at once. or have a makefile or some such where its explicit
