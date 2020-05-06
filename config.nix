{ nur ? import ~/personal/nur-packages {}
}:
rec {

  ghcVersion = "ghc882";

  yugioh-overlay = self: super:
    let

      dontCheck = super.haskell.lib.dontCheck;

      haskellOverlay = selfH: superH: {
        ghc-tcplugins-extra = selfH.callHackage "ghc-tcplugins-extra" "0.3.2" {};
        polysemy = selfH.callHackage "polysemy" "1.3.0.0" {};
      };

    in
      {
        haskellPackages = super.haskellPackages.extend haskellOverlay;
        haskell = super.haskell // {
          inherit ghcVersion;
          packages = super.haskell.packages // {
            "${ghcVersion}" =
              super.haskell.packages.${ghcVersion}.extend haskellOverlay;
          };
        };
      };

  nixpkgsRev = "694ac1b127ce34fb3bf5890232e1375b3911867c";

  pkg = {
    name = "yugioh";
    path = ./.;
    args = {};
  };

}
