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
      ../../modules/shared-home.nix
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

  # Enable firmware with contentious licensing
  hardware.enableAllFirmware = true;

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable my desktop manager of choice.
  services.desktopManager.gnomeEvan = {
    enable = true;
    monitorConfig = ./monitors.xml;
    user = "epetousis";
  };

  # Enable my general NixOS settings.
  system.epetousis.enable = true;
  system.epetousis.enableExtras = true;

  # Enable the (unfortunately proprietary) Nvidia driver.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;
  hardware.graphics.extraPackages = with pkgs; [ nvidia-vaapi-driver ];
  hardware.nvidia.open = false;
  hardware.nvidia.gsp.enable = false;
  # Enable systemd-based power management (should be enabled by default)
  hardware.nvidia.powerManagement.enable = true;

  boot.kernelParams = [
    # Enable experimental framebuffer console support
    "nvidia_drm.fbdev=1"
  ];

  # Enable Steam
  # NB: it is *essential* that you restart Steam after switching generations, otherwise Proton will fail to work.
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
    package = pkgs.steam.override {
      extraArgs = "-pipewire -pipewire-dmabuf";
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

  # Make Steam match Gnome's internal resolution for Xwayland (2x), which will then be downscaled to 1.5x to match my monitor.
  environment.sessionVariables.STEAM_FORCE_DESKTOPUI_SCALING = "2.0";

  # Apply 2x scaling on Xwayland cursor to match internal resolution, to be downscaled to 1.5x
  environment.sessionVariables.XCURSOR_SIZE = "48";

  # Enable Game Mode.
  programs.gamemode.enable = true;

  # Virtual machine support.
  programs.virt-manager.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      ovmf = {
        enable = true;
        packages = [ pkgs.OVMFFull.fd ];
      };
      swtpm = {
        enable = true;
      };
    };
  };

  # Enable ALVR.
  programs.alvr = {
    enable = true;
    openFirewall = true;
  };

  # Enable adb.
  programs.adb.enable = true;

  # Enable arrpc for Discord RPC via Home Manager, since there is no nixpkgs service.
  home-manager.users.epetousis.services.arrpc.enable = true;

  # Add Sunshine.
  services.sunshine = {
    enable = true;
    capSysAdmin = true;
    openFirewall = true;
  };

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
  };
  home-manager.users.epetousis.programs.ssh.matchBlocks."*".extraOptions.IdentityAgent = pkgs.lib.mkForce "~/.1password/agent.sock";

  users.users.epetousis.extraGroups = [
    "adbusers" # On this machine only, add my user to adbusers.
    "libvirt" # Add my user to libvirt's group.
    "plugdev"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    steamtinkerlaunch # Don't forget to link the compatibility tool: https://gist.github.com/jakehamilton/632edeb9d170a2aedc9984a0363523d3#install
    gamescope
    gamemode
    mangohud
    rpcs3
    solaar
    libreoffice
    thunderbird
    vesktop
    keymapp
    blender
  ];

  # Apply udev rules from packages.
  services.udev.packages = with pkgs; [
    rpcs3 # Needed so that USB gamepads can be detected
    solaar # Needed to detect connected mice
  ];

  services.udev.extraRules = ''
  # Keymapp / Wally Flashing rules for the Moonlander and Planck EZ
  SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666", SYMLINK+="stm32_dfu"
  # Rules for Oryx web flashing and live training
  KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
  KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"
  '';

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
