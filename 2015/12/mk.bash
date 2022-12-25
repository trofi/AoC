#!/usr/bin/env bash

set -e

# needs re2c's re2rust
re2rust -W main.re -o main.rs
rustc main.rs
./main
