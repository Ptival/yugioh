let

  name = "yugioh";
  compiler-nix-name = "ghc883";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = import (fetchNiv "hackage.nix");
  };
  all-hies = import (fetchNiv "all-hies") {};

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      all-hies.overlay
    ];
  });

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    modules = [

      # {
      #   reinstallableLibGhc = true;
      # }

      {
        # Make Cabal reinstallable
        nonReinstallablePkgs = [

          "array"
          "base"
          "binary"
          "bytestring"
          # "Cabal"
          "containers"
          "deepseq"
          "directory"
          "filepath"
          "ghc"
          "ghc-prim"
          "hpc"
          "integer-gmp"
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
      cabal = {
        inherit (nix-compiler-name);
        version = "3.2.0.0";
      };
      hie = "unstable";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
