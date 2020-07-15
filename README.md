# dk2agda

A tool to convert files written in Dedukti to Agda

## Dependencies

It depends heavily on lambdapi (hence all of its dependencies are required).

## Build

To build the binaries, use the following :

```sh
make install
```

## Scripts

### dk2agda.ml

Main export script. It takes a dk/lp file and an output directory as arguments and returns the new agda file in the output directory.
Note that with this script, the output dir must exist beforehand.

### dk2agda.sh

Utility script that will, later, handle the arguments, files, directories, etc, for dk2agda.ml.
It is a more intuitive way to use dk2agda since the output directory will be created if it doesn't exist and one can pass multiple files located in a single directory at once.

## Sttfa.{agda|dk}

out/sttfa.agda is the traduction of test\_files/sttfa/sttfa.dk written by dk2agda.ml.

test\_files/sttfa/sttfa.agda is the traduction of test\_files/sttfa/sttfa.dk written by hand.

None of them type-check yet. (using `agda sttfa.agda`)
