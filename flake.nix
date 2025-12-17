{
  description = "Development environment for Advent Of Code";
  inputs = {
    # use system flake to avoid redownload and match closer software versions
    nixpkgs.url = "flake:nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.rustc
        pkgs.cargo
        pkgs.clippy
        pkgs.evcxr

        pkgs.re2c
      ];
    };
  });
}
