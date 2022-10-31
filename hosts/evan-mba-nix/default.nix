# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, m1-support, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Include the necessary packages and configuration for Apple M1 support.
      # M1 kernel builds currently require a cross-compiled stdenv.
      (import (m1-support + "/nix/m1-support"))
      ../../modules/m1-firmware
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;

  # Remove if you get an error that an x86_64-linux builder is required.
  # boot.kernelBuildIsCross = true;

  networking.hostName = "evan-mba-nix"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;

  # Map caps lock to control in Gnome
  # Caveat: this only works as a defaults override. Gnome sets this as `['terminate:ctrl_alt_bksp']' by default - you must reset with gsettings reset.
  services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
     [org.gnome.desktop.input-sources]
     xkb-options=['caps:ctrl_modifier', 'terminate:ctrl_alt_bksp']
   '';

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable Bluetooth.
  hardware.bluetooth.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Allow unfree packages.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ../../overlays)
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.epetousis = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      (firefox-wayland.override { extraNativeMessagingHosts = [ gnomeExtensions.gsconnect ]; })
      webcord
  #     thunderbird
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ== evan@petousis.net"
    ];
  };

  home-manager.users.epetousis = import ../../modules/home.nix;

  # Allow Nix command
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
    gnomeExtensions.gsconnect
    gnomeExtensions.appindicator
  ];

  # Enable iOS USB support
  services.usbmuxd.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8080 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Allow port ranges
  networking.firewall.allowedTCPPortRanges = [
    { from = 1714; to = 1764; }  # KDE Connect
  ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 1714; to = 1764; }  # KDE Connect
  ];

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

