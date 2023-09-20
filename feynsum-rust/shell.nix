{ pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv
}:

pkgs.mkShell {
  buildInputs = [ pkgs.cargo pkgs.rustc ];
}
