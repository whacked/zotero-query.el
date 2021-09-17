{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.jsonnet
    pkgs.nodejs
    pkgs.yarn
  ];

  shellHook = ''
    export PATH=$PWD/typescript/node_modules/.bin:$PATH
  '';
}
