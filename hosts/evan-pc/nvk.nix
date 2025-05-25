{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.epetousis.nvk;
in {
  options.system.epetousis.nvk = {
    enable = mkEnableOption "Enable Nouveau with NVK, with some tweaks for usability improvements.";
  };

  config = mkIf cfg.enable {
    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = true;

    boot.kernelParams = [
      # Make sure the GSP is used.
      "nouveau.config=NvGspRm=1"
    ];
  };
}
