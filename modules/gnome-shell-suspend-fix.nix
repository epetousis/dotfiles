/* This is a hack fix for Nvidia drivers.
There's currently a bug relating to suspend on Nvidia causing graphical corruption: https://forums.developer.nvidia.com/t/corrupted-graphics-upon-resume-gnome-41-x-org-495-44-driver/194565/17
The workaround for this bug is to make the Nvidia driver save all VRAM instead of a small portion via `hardware.nvidia.powerManagement.enable = true`. However, gnome-shell causes suspend to fail by communicating with the driver after the driver itself is suspended. See: https://bbs.archlinux.org/viewtopic.php?id=277713 and https://forums.developer.nvidia.com/t/trouble-suspending-with-510-39-01-linux-5-16-0-freezing-of-tasks-failed-after-20-009-seconds/200933/12
The solution for this is to interrupt gnome-shell on suspend. */

{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.gnome-shell-suspend-fix;
in {
  options.services.gnome-shell-suspend-fix = {
    enable = mkEnableOption "Enables the gnome-shell suspend hack fix.";
  };

  config = mkIf cfg.enable {
    systemd.services.gnome-shell-suspend = {
      description = "Suspend gnome-shell";
      wantedBy = [ "systemd-suspend.service" "systemd-hibernate.service" ];
      before = [
        "systemd-suspend.service"
        "systemd-hibernate.service"
        "nvidia-suspend.service"
        "nvidia-hibernate.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.killall}/bin/killall -STOP .gnome-shell-wrapper";
      };
    };

    systemd.services.gnome-shell-resume = {
      description = "Resume gnome-shell";
      wantedBy = [ "systemd-suspend.service" "systemd-hibernate.service" ];
      after = [
        "systemd-suspend.service"
        "systemd-hibernate.service"
        "nvidia-resume.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.killall}/bin/killall -CONT .gnome-shell-wrapper";
      };
    };
  };
}
