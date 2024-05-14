{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.desktopManager.gnomeEvan;
in {
  options.services.desktopManager.gnomeEvan = {
    enable = mkEnableOption "Enables my (Evan's) Gnome specific config.";
    monitorConfig = mkOption {
      type = types.nullOr types.path;
      description = "A path to a gdm XML monitor config.";
      default = null;
    };
  };

  config = mkIf cfg.enable {
    # Enable the X11 windowing system.
    services.xserver = {
      enable = true;
      # Enable Gnome.
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };

    # Enable fractional scaling on Gnome Wayland and disable switch monitor keybind.
    services.xserver.desktopManager.gnome.extraGSettingsOverridePackages = [ pkgs.gnome.mutter ];
    services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
      [org.gnome.mutter]
      experimental-features=['scale-monitor-framebuffer']
      [org.gnome.mutter.keybindings]
      switch-monitor=[]
    '';

    # Enable Wayland by default in Chromium apps
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    # Install some essential extensions
    environment.systemPackages = with pkgs; [
      gnome.gnome-themes-extra
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.pop-shell
      gnomeExtensions.night-theme-switcher
      gnomeExtensions.user-themes
      gnomeExtensions.vitals
      gnomeExtensions.display-scale-switcher
    ];

    # Link our monitor config to gdm's config directory, so we can do things like have a high refresh rate login screen!
    systemd.tmpfiles.rules = lib.mkIf (cfg.monitorConfig != null) [
      ''L+ /run/gdm/.config/monitors.xml - gdm gdm - ${cfg.monitorConfig}''
    ];

    # Enable iOS device support
    services.usbmuxd.enable = true;

    # Enable KDE Connect
    programs.kdeconnect = {
      enable = true;
      package = pkgs.gnomeExtensions.gsconnect;
    };
  };
}
