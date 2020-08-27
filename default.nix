let

  name = "yugioh";
  compiler-nix-name = "ghc883";

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {};
  all-hies = import (fetchTarball { inherit (sources.all-hies) url sha256; }) {};

  pkgs =
    (import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        all-hies.overlay
      ];
    }));

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    modules = [

      {
        reinstallableLibGhc = true;
      }

      {
        # Make Cabal reinstallable?
        nonReinstallablePkgs = [

          "Win32"
          "array"
          "array"
          "base"
          "binary"
          "bytestring"
          "containers"
          "deepseq"
          "directory"
          "filepath"
          "ghc"
          "ghc-boot"
          "ghc-boot"
          "ghc-boot-th"
          "ghc-compact"
          "ghc-heap"
          "ghc-prim"
          "ghc-prim"
          "ghcjs-prim"
          "ghcjs-th"
          "hpc"
          "integer-gmp"
          "integer-simple"
          "mtl"
          "parsec"
          "pretty"
          "process"
          "rts"
          "template-haskell"
          "terminfo"
          "text"
          "time"
          "transformers"
          "unix"
          "xhtml"

        ];
      }

    ];

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

set.${name}.components.library // {

  shell = set.shellFor {

    exactDeps = true;

    packages = p: [
      p.yugioh
    ];

    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';

    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
      # hlint = "2.2.11";
      hpack = "0.34.2";
      # ormolu = "0.1.2.0";
    };

  };

}
