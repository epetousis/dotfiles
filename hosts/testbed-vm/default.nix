# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, modulesPath, ... }:

{
  imports =
    [
      "${toString modulesPath}/profiles/qemu-guest.nix"
      # Creates a new output, nixosConfigurations.testbed-vm.config.system.build.image
      "${toString modulesPath}/virtualisation/disk-image.nix"
    ];

  image = {
    format = "qcow2";
    efiSupport = true;
  };

  nix.settings.trusted-public-keys = [ "evan-mbp:VD4qrQtKpdF9rxno4j65xsSGGlsELchErIHZGfzmU90=" ];

  networking.hostName = "testbed-vm";

  # Enable networking
  networking.networkmanager.enable = true;

  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  users.users.epetousis = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    initialPassword = "test";
    packages = with pkgs; [
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKoWt6O7x9DP5S9BuoqgWJI2WrW57TqqamFpz9w/Tquj evan@petousis.net"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
  ];

  xdg.mime.defaultApplications = {
    "text/html" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
