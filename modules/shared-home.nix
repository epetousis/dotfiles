{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.epetousis;
in {
  options.system.epetousis = {
    enableExtras = mkEnableOption "Enables my (Evan's) shared NixOS PC options for home specific extras.";
  };

  config = mkIf cfg.enable {
    # Add a localhost.local hosts record.
    networking.hosts = {
      "127.0.0.1" = [ "localhost.local" ];
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    services.pipewire = {
      # Enable AirPlay in Pipewire
      # FIXME: no sound is ever actually output
      extraConfig.pipewire."91-airplay" = {
        "context.modules" = [
          {
            name = "libpipewire-module-raop-discover";
          }
        ];
      };
    };

    users.users.epetousis = {
      extraGroups = [
        "syncthing" # Allow access to files created and updated by the Syncthing daemon.
      ];
      packages = with pkgs; [
        axel
        deluge
        drawio
        easyeffects
        libreoffice-fresh
        mixxx
        obs-studio
        signal-desktop
        yt-dlp
      ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ== evan@petousis.net"
      ];
    };

    # System-wide packages
    environment.systemPackages = with pkgs; [
      tailscale-systray
    ];

    # Apply udev rules from packages.
    services.udev.packages = [
      pkgs.mixxx # Needed to be able to detect USB DJ controllers
    ];

    # Enable Tailscale service
    services.tailscale = {
      enable = true;
      openFirewall = true;
      useRoutingFeatures = "client";
    };

    fonts.packages = with pkgs; [
      apple-color-emoji
      cantarell-fonts
      inter
    ];

    fonts.fontconfig.defaultFonts.emoji = [ "Apple Color Emoji" ];

    services.rcloneMount = {
      enable = true;
      username = "epetousis";
    };

    # Enable the OpenSSH daemon.
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };

    # Enable flatpak support
    services.flatpak.enable = true;

    # Ensure Gnome Keyring is present.
    services.gnome.gnome-keyring.enable = true;
    programs.seahorse.enable = true;

    # Enable Syncthing for syncing my home storage.
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
    };

    # Enable iOS device support
    services.usbmuxd.enable = true;

  };
}
