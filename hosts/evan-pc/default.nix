# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
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
  networking.hosts = {
    "127.0.0.1" = [ "localhost.local" ];
  };

  # Enable Bluetooth
  hardware.bluetooth.enable = true;

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.utf8";

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.sddm.wayland.enable = true;
  };

  # Enable KDE Plasma 6.
  services.desktopManager.plasma6.enable = true;

  # Enable the (unfortunately proprietary) Nvidia driver.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  # Enable systemd-based power management (should be enabled by default)
  hardware.nvidia.powerManagement.enable = true;

  # Enable Wayland on Nvidia.
  hardware.nvidia.modesetting.enable = true;

  # Enable Wayland support for apps.
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Enable Xwayland for X compatibility.
  programs.xwayland.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "au";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  users.users.epetousis = {
    isNormalUser = true;
    description = "Evangelos Petousis";
    extraGroups = [ "wheel" ];
    packages = with pkgs; [
      beeper
      bottles
      drawio
      easyeffects
      jetbrains.idea-ultimate
      libreoffice-fresh
      obs-studio
      signal-desktop
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ== evan@petousis.net"
    ];
  };

  # Enable Steam
  # NB: it is *essential* that you restart Steam after switching generations, otherwise Proton will fail to work.
  programs.steam.enable = true;

  # Enable Game Mode.
  programs.gamemode.enable = true;

  # Enable flatpak support
  services.flatpak.enable = true;

  # Enable iOS device support
  services.usbmuxd.enable = true;

  # Enable zsh
  programs.zsh.enable = true;

  # Enable KDE Connect
  programs.kdeconnect.enable = true;

  home-manager.users.epetousis = import ../../modules/home.nix;
  home-manager.extraSpecialArgs = { applyTheme = false; };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ../../overlays)
  ];

  # Allow Nix command
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    openai-whisper-cpp
    podman-compose
    chromium
  ];

  # Services:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  # Enable Tailscale service
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  # Enable Podman
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };

  services.rcloneMount = {
    enable = true;
    username = "epetousis";
  };

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
