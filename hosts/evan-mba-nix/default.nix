{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  # Asahi-specific settings
  hardware.asahi.useExperimentalGPUDriver = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "evan-mba"; # Define your hostname.
  time.timeZone = "Australia/Melbourne";
  i18n.defaultLocale = "en_AU.utf8";
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;

  services.desktopManager.plasma6.enable = true;
  programs.kdeconnect.enable = true;

  # Enable Wayland by default in Chromium apps
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.zsh.enable = true;
  users.users.epetousis = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      # Chromium needed for work, don't give this to everyone
      chromium
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ== evan@petousis.net"
    ];
  };
  environment.sessionVariables.MOZ_GMP_PATH = [ "${pkgs.widevine-cdm-lacros}/gmp-widevinecdm/system-installed" ];

  home-manager.users.epetousis.imports = [
    ../../modules/home.nix
  ];

  # System-wide packages
  environment.systemPackages = with pkgs; [
    asahi-btsync
    libsForQt5.polonium
  ];

  # Unlock GPG keys on login
  security.pam.services.login.gnupg.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

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

