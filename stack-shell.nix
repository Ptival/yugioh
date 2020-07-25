{ nur ? import ~/personal/nur-packages {}
}:
let
  config = import ./config.nix;
in
nur.lib.stackShell {

  inherit (config) ghcVersion nixpkgsRev pkg;

  nixpkgsArgs = {
    overlays = [
      # (nur.lib.applyHaskellOverlay config.ghcVersion config.yugioh-overlay)
    ];
  };

}
