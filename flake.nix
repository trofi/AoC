{
  description = "Development environment for Advent Of Code";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # not merged yet:
    nixpkgs_re2c_4.url = "git+https://github.com/NixOS/nixpkgs?ref=refs/pull/357342/head";
  };
  outputs = { self, nixpkgs, flake-utils, nixpkgs_re2c_4 }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
    };
    pkgs_re2c_4 = import nixpkgs_re2c_4 {
      inherit system;
    };
  in {
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.rustc
        pkgs.cargo
        pkgs.evcxr

        #pkgs.re2c
        pkgs_re2c_4.re2c
      ];
    };
  });
}
