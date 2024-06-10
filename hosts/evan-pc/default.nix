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
    v4l2loopback
    # Add Logitech G923 (PS) drivers
    new-lg4ff
  ];

  # Use Zen Linux kernel.
  boot.kernelPackages = pkgs.linuxPackages_zen;

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
  hardware.opengl.extraPackages = with pkgs; [ nvidia-vaapi-driver ];
  # Enable systemd-based power management (should be enabled by default)
  hardware.nvidia.powerManagement.enable = true;

  # Enable Wayland on Nvidia.
  hardware.nvidia.modesetting.enable = true;

  # Enable Nvidia driver 555 to fix Xwayland flickering.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;

  boot.kernelParams = [
    # FIXME: remove when GSP firmware doesn't cause Firefox Wayland to crash on explicit sync
    "nvidia.NVreg_EnableGpuFirmware=0"
    # Enable experimental framebuffer console support
    "nvidia_drm.fbdev=1"
  ];

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

  # Enable Waydroid.
  virtualisation.waydroid.enable = true;

  # Make Steam match my monitor scaling on Plasma.
  environment.sessionVariables.STEAM_FORCE_DESKTOPUI_SCALING = "1.5";

  # Apply 1.5x scaling on Xwayland cursor to match primary display
  environment.sessionVariables.XCURSOR_SIZE = "36";

  # Enable Game Mode.
  programs.gamemode.enable = true;

  # Enable ALVR.
  programs.alvr = {
    enable = true;
    openFirewall = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    openai-whisper-cpp
    gamescope
    gamemode
    mangohud
    zoom-us
    rpcs3
    solaar
    streamdeck-ui
  ];

  # Apply udev rules from packages.
  services.udev.packages = with pkgs; [
    rpcs3 # Needed so that USB gamepads can be detected
    solaar # Needed to detect connected mice
    streamdeck-ui # Needed to connect to Stream Decks
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
