{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/gnome.nix
      ../../modules/shared.nix
      ../../modules/rclone-mount.nix
    ];

  # Asahi-specific settings
  hardware.asahi.useExperimentalGPUDriver = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "evan-mba"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };

  networking.wireless.iwd = {
    enable = true;
    settings.General.EnableNetworkConfiguration = true;
  };

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # Enable my Gnome config.
  services.desktopManager.gnomeEvan = {
    enable = true;
  };

  # Enable my general NixOS settings.
  system.epetousis.enable = true;

  # System-wide packages
  environment.systemPackages = with pkgs; [
    asahi-btsync
    tailscale-systray
    mixxx
    gtk-engine-murrine
  ];

  # Make sure Mixxx has permission to access USB devices
  services.udev.packages = [ pkgs.mixxx ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.05"; # Did you read the comment?

}

