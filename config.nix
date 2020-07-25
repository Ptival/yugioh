{

  ghcVersion = "ghc882";

  yugioh-overlay = self: super:
    let
      dontCheck = super.haskell.lib.dontCheck;
    in
      selfH: superH: {
        ghc-tcplugins-extra = selfH.callHackage "ghc-tcplugins-extra" "0.3.2" {};
        polysemy = selfH.callHackage "polysemy" "1.3.0.0" {};
      };

  nixpkgsRev = "571212eb839d482992adb05479b86eeb6a9c7b2f"; # Updated May 25th 2020

  pkg = {
    name = "yugioh";
    path = ./.;
    args = {};
  };

}
