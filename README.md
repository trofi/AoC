These are my attempts at solving <https://adventofcode.com/> puzzles.

## How to run

Most of the solutions are in `rust` and should be runnable as:

```
$ rustc -O main.rs && ./main
```

Very occasionally there is an extra dependency on `re2c` or on a `cargo`
package.

## Nix development shell

`flake.nix` also allows you to drop into a shell with all required
prerequisites. As I don't track `flake.lock` and explicitly ignore it in
`.gitignore` you will need an extra `--no-write-lock-file` flag:

```
$ nix develop --no-write-lock-file
```

I personally use the following command to use `nixpkgs` version from my
running system:

```
$ nix develop --no-write-lock-file --override-input nixpkgs flake:nixpkgs
```
