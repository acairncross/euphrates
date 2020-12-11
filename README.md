# Euphrates

Euphrates is a hardware implementation of the parallel push-relabel algorithm
for maximum flow described by Bo Hong in ["A Lock-free Multi-threaded Algorithm
fot the Maximum Flow Problem"](http://people.cs.ksu.edu/~wls77/weston/projects/cis598/hong.pdf).

## Build

To generate Verilog, build the library to produce a GHC environment file for
Clash to use, then run Clash:

```sh
cabal build --write-ghc-environment-files=always euphrates
cabal run clash -- Euphrates.Top --verilog
```

Or run the Shake based build system to run Clash and synthesize a bitstream for
the ULX3S:

```sh
cabal run shake
```
