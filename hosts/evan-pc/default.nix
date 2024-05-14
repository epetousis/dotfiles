# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../modules/gnome.nix
      ../../modules/shared.nix
      ../../modules/rclone-mount.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Add custom kernel modules
  boot.extraModulePackages = with config.boot.kernelPackages; [
    # Add video loopback for software-applied webcam effects
    v4l2loopback.out
  ];

  networking.hostName = "evan-pc"; # Define your hostname.

  # Enable Bluetooth
  hardware.bluetooth.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable my Gnome config.
  services.desktopManager.gnomeEvan = {
    enable = true;
    monitorConfig = ./monitors.xml;
  };

  # Enable my general NixOS settings.
  system.epetousis.enable = true;

  # Enable the (unfortunately proprietary) Nvidia driver.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  # Enable systemd-based power management (should be enabled by default)
  hardware.nvidia.powerManagement.enable = true;

  # Enable Wayland on Nvidia.
  hardware.nvidia.modesetting.enable = true;

  # Enable Steam
  # NB: it is *essential* that you restart Steam after switching generations, otherwise Proton will fail to work.
  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      extraEnv = {};
      extraLibraries = pkgs: with pkgs; [
        xorg.libXcursor
        xorg.libXi
        xorg.libXinerama
        xorg.libXScrnSaver
        libpng
        libpulseaudio
        libvorbis
        stdenv.cc.cc.lib
        libkrb5
        keyutils
      ];
    };
    gamescopeSession = {
      enable = true;
      args = [
        "--rt"
        "-f"
        "-o 10"
      ];
    };
  };

  # Enable Game Mode.
  programs.gamemode.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    openai-whisper-cpp
    gamescope
    gamemode
    mangohud
  ];

  users.users.epetousis.packages = with pkgs; [
    beeper
    (bottles.override (p: { extraPkgs = bPackages: [ bPackages.wineasio ]; }))
    jetbrains.idea-ultimate
  ];

  # Services:

  programs.java = {
    enable = true;
    package = pkgs.jdk22;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
