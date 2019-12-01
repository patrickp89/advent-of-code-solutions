# advent-of-code-solutions
This repository contains my solutions to the annual [Advent of Code](https://adventofcode.com/) puzzles.

## How to compile the solutions
I used OCaml for the puzzles (I solved so far). To build (and run) the solutions, install [OPAM](https://opam.ocaml.org/doc/Install.html), the [OCaml](https://ocaml.org/docs/install.html) compiler, and GNU m4. Then initialise OPAM. On Debian run:
```bash
# apt-get install opam ocaml m4
$ opam init
$ eval $(opam config env)
```

Then install Jane Street's [Dune](https://dune.build/), [Core](https://opensource.janestreet.com/core/), and expectation-test libraries via OPAM by running:
```bash
$ opam install dune core ppx_expect
```

You can then build the solutions:
```bash
$ dune build
```

## How to run the solutions
> For copyright reasons I do *not* commit the AOC puzzle input files here!

In order to run the solutions you therefore have to download the original input files first! Log in to the AOC page with an account of your choice and download the input files. Save them in the appropriate year/day/XY/ folder, e.g. download [the first](https://adventofcode.com/2019/day/1/input) input file for 2019 and save it as
```bash
2019/day/01/2019-01.input
```

Then change into the folder for that very year and run the solutions:
```bash
$ cd 2019
$ dune runtest
```
