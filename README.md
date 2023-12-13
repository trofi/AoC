These are my attempts at solving <https://adventofcode.com/> puzzles.

## How to run

Most of the solutions are in `rust` and should be runnable as:

```
$ rustc -O main.rs && ./main
```

Very occasionally there is an extra dependency on `re2c` or on a `cargo`
package.

## `nix` development shell

This repository contains `flake.nix` file. It allows you to drop into a
development shell with all required prerequisites.

There is one caveat: I don't track `flake.lock` and explicitly ignore it
in `.gitignore`. You will need an extra `--no-write-lock-file` flag to
run it successfully:

```
$ nix develop --no-write-lock-file
```

I personally use the following command to use `nixpkgs` version from my
running system:

```
$ nix develop --no-write-lock-file --override-input nixpkgs flake:nixpkgs
```
