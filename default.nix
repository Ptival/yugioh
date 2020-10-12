let

  name = "yugioh";
  compiler-nix-name = "ghc883";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = {
      hackageSrc = fetchNiv "hackage.nix";
    };
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
          "ghc-boot"
          "ghc-boot-th"
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

    pkg-def-extras = [
      (hackage: {
        packages = {
          fourmolu = hackage.fourmolu."0.1.0.0".revisions.default;
          # haskell-lsp-types = hackage.haskell-lsp-types."0.22.0.0".revisions.default;
          # actionBracket was added in shake-0.18.4
          shake = hackage.shake."0.18.4".revisions.default;
        };
      })
    ];

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

set.${name}.components.library // {

  shell = set.shellFor {

    buildInputs = with set; [
      set.ghcide.components.exes.ghcide
      # haskell-language-server.components.library
      # haskell-language-server.components.exes.haskell-language-server
      haskell-language-server.components.exes.haskell-language-server-wrapper
    ];

    exactDeps = true;

    packages = p: [
      # p.brittany
      # p.ghcide # crashes nix-shell!?
      # p.haskell-language-server
      # p.optparse-applicative
      p.yugioh
    ];

    # shellHook = ''
    #   export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    # '';

    tools = {
      cabal = {
        inherit (nix-compiler-name);
        version = "3.2.0.0";
      };
      # hie = "unstable";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
