{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.cargo pkgs.rust-analyzer pkgs.rustc pkgs.rustfmt pkgs.clippy
    pkgs.libiconv
  ];
}
