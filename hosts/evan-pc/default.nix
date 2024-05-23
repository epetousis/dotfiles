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

  # Enable my Plasma config.
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };

  services.desktopManager.plasma6.enable = true;
  programs.kdeconnect.enable = true;

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

  # Enable Nvidia driver 555 to fix Xwayland flickering.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
    version = "555.42.02";
    sha256_64bit = "sha256-k7cI3ZDlKp4mT46jMkLaIrc2YUx1lh1wj/J4SVSHWyk=";
    sha256_aarch64 = "sha256-rtDxQjClJ+gyrCLvdZlT56YyHQ4sbaL+d5tL4L4VfkA=";
    openSha256 = "sha256-rtDxQjClJ+gyrCLvdZlT56YyHQ4sbaL+d5tL4L4VfkA=";
    settingsSha256 = "sha256-rtDxQjClJ+gyrCLvdZlT56YyHQ4sbaL+d5tL4L4VfkA="; 
    persistencedSha256 = "sha256-3ae31/egyMKpqtGEqgtikWcwMwfcqMv2K4MVFa70Bqs=";
  };

  # Enable Steam
  # NB: it is *essential* that you restart Steam after switching generations, otherwise Proton will fail to work.
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
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
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
