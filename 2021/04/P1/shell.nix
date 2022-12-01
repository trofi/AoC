{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      (ghc.withPackages (ps: [ps.split]))
    ];

    shellHook = ''
      exec ghci main.hs
    '';
  }
