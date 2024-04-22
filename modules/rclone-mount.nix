{ config, lib, pkgs, ... }:

let
  cfg = config.services.rcloneMount;
in
{
  options.services.rcloneMount = {
    enable = lib.mkEnableOption "Enable rclone mount for iCloud Org folder";

    username = lib.mkOption {
      type = lib.types.str;
      default = "yourUsername";
      description = "Username of the user for which the rclone mount is configured.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.rclone ];

    fileSystems."/home/${cfg.username}/Documents/iCloudOrg" = {
      fsType = "rclone";
      device = "iclouddrive:/Critical/Org";
      options = [
        "rw"
        "noauto"
        "nofail"
        "x-systemd.automount"
        "args2env"
        "config=/home/${cfg.username}/.config/rclone/rclone.conf"
        "cache_dir=/var/cache/rclone"
        "vfs-cache-mode=full"
        "allow-other"
        "_netdev"
      ];
      neededForBoot = false;
    };
  };
}
