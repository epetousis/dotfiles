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
    user = mkOption {
      type = types.str;
      description = "The user account on which to apply GTK settings to.";
    };
    macStyleFonts = mkOption {
      type = types.bool;
      description = "Whether to enable macOS style font rendering.";
      default = true;
    };
  };

  config = lib.mkMerge [
  (mkIf cfg.enable {
    # Enable the X11 windowing system.
    services.xserver = {
      enable = true;
      # Enable Gnome.
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };

    # Enable fractional scaling on Gnome Wayland and disable switch monitor keybind.
    services.xserver.desktopManager.gnome.extraGSettingsOverridePackages = [ pkgs.mutter ];
    services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
      [org.gnome.mutter]
      experimental-features=['scale-monitor-framebuffer', 'xwayland-native-scaling']
      [org.gnome.mutter.keybindings]
      switch-monitor=[]
      [org.gnome.desktop.interface]
      font-rendering='manual'
      font-hinting='none'
      font-antialiasing='grayscale'
    '';

    # Install some essential extensions
    environment.systemPackages = with pkgs; [
      gnome-themes-extra
      gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.night-theme-switcher
      gnomeExtensions.dash-to-dock
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

    # Enable Gnome remote desktop
    services.gnome.gnome-remote-desktop.enable = true;
  })
  (lib.mkIf cfg.macStyleFonts {
    # Disable hinting
    fonts.fontconfig.hinting.enable = false;

    # Disable subpixel rendering
    fonts.fontconfig.subpixel.rgba = "none";
    fonts.fontconfig.subpixel.lcdfilter = "none";

    # Enable stem darkening
    environment.variables.FREETYPE_PROPERTIES="cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
  })
  # Disable hinting
  (lib.mkIf (cfg.macStyleFonts && config.home-manager != null && cfg.user != null) {
    home-manager.users.${cfg.user} = {
      gtk = {
        gtk4.extraConfig = {
          gtk-hint-font-metrics = false;
        };
      };
    };
  })
  ];
}
