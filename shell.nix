{ nur ? (import ~/personal/nur-packages {})
}:
let
  config = import ./config.nix;
  haskell-dev-overlay = nur.overlays.haskell-dev.${config.ghcVersion};
in
nur.lib.haskellDevShell {

  inherit (config) ghcVersion nixpkgsRev pkg;

  nixpkgsArgs = {
    overlays = [
      haskell-dev-overlay
      (nur.lib.applyHaskellOverlay config.ghcVersion config.yugioh-overlay)
    ];
  };

}
