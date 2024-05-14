{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.epetousis;
in {
  options.system.epetousis = {
    enable = mkEnableOption "Enables my (Evan's) shared NixOS PC options.";
  };

  config = mkIf cfg.enable {
    # Set locale defaults.
    time.timeZone = "Australia/Melbourne";
    i18n.defaultLocale = "en_AU.utf8";

    # Allow unfree packages in Nix
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      (import ../overlays)
    ];

    # Allow Nix command
    nix.settings.experimental-features = [ "nix-command" "flakes" ];

    # Add a localhost.local hosts record.
    networking.hosts = {
      "127.0.0.1" = [ "localhost.local" ];
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable sound, computers that I use frequently need sound enabled.
    sound.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      # Enable AirPlay
      # FIXME: no sound is ever actually output
      extraConfig.pipewire."91-airplay" = {
        "context.modules" = [
          {
            name = "libpipewire-module-raop-discover";
          }
        ];
      };
    };

    # Enable Xwayland for X compatibility.
    programs.xwayland.enable = true;

    # Define a user account. Don't forget to set a password with ‘passwd’.
    programs.zsh.enable = true;
    users.users.epetousis = {
      description = "Evangelos Petousis";
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
      packages = with pkgs; [
        # Chromium needed for work, don't give this to everyone
        (chromium.override {
          commandLineArgs = [
            "--enable-features=TouchpadOverscrollHistoryNavigation"
          ];
        })
        drawio
        easyeffects
        libreoffice-fresh
        obs-studio
        signal-desktop
      ];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ== evan@petousis.net"
      ];
    };
    environment.sessionVariables.MOZ_GMP_PATH = [ "${pkgs.widevine-cdm-lacros}/gmp-widevinecdm/system-installed" ];

    home-manager.users.epetousis = import ./home.nix;

    # System-wide packages
    environment.systemPackages = with pkgs; [
      tailscale-systray
      podman-compose
    ];

    # Enable Podman with `docker` alias
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
    };

    # Enable Tailscale service
    services.tailscale = {
      enable = true;
      openFirewall = true;
      useRoutingFeatures = "client";
    };

    fonts.packages = with pkgs; [
      apple-color-emoji
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

    # Enable Japanese keyboard support
    i18n.inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ mozc ];
    };

    # Enable systemd-oomd extra services
    systemd.oomd = {
      # See https://www.freedesktop.org/software/systemd/man/latest/systemd-oomd.service.html#Usage%20Recommendations for why these are enabled
      enableSystemSlice = true;
      enableUserServices = true;
    };
  };
}
