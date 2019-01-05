{ nixpkgs ? import <nixpkgs> {}
}:
with nixpkgs;
mkShell {
  buildInputs = [
    cabal-install
    stack
  ];
  inputsFrom = [
    (haskellPackages.callPackage ./default.nix {})
  ];
  FOO="bar";
}
