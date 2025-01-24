{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.epetousis.nvidia;
in {
  options.system.epetousis.nvidia = {
    enable = mkEnableOption "Enable Nvidia's proprietary driver, with some tweaks for usability improvements.";
  };

  config = mkIf cfg.enable {
    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = true;

    # Enable the (unfortunately proprietary) Nvidia driver.
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.graphics.extraPackages = with pkgs; [ nvidia-vaapi-driver ];
    hardware.nvidia.open = false;
    # Enable systemd-based power management (should be enabled by default)
    hardware.nvidia.powerManagement.enable = true;

    /* Disable systemd-sleep's user session freezing behaviour - seems like
  a good idea in practice, but incompatible with Nvidia's proprietary
  drivers. */
    systemd.services.systemd-suspend.environment.SYSTEMD_SLEEP_FREEZE_USER_SESSIONS = "false";

    boot.kernelParams = [
      # Enable experimental framebuffer console support
      "nvidia_drm.fbdev=1"
    ];
  };
}
