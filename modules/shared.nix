{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.epetousis;
in {
  options.system.epetousis = {
    enable = mkEnableOption "Enables my (Evan's) shared NixOS PC options for non-negotiable settings.";

    useHome = mkOption {
      type = types.bool;
      default = true;
      description = lib.mdDoc ''
        Whether to include my home-manager configuration.
      '';
    };
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

    # Add kernel flags
    boot.kernelParams = [
      # Silent boot
      "quiet"
      # Disable systemd logs
      "udev.log_level=3"
    ];
    boot.initrd.verbose = false;

    # Allow Nix command
    nix.settings.experimental-features = [ "nix-command" "flakes" ];

    # Make Lix trust admin users
    nix.settings.trusted-users = [
      "root"
      "@wheel"
    ];

    # Add a localhost.local hosts record.
    networking.hosts = {
      "127.0.0.1" = [ "localhost.local" ];
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable Pipewire
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    # Enable Xwayland for X compatibility.
    programs.xwayland.enable = true;

    # Enable Wayland by default in Chromium apps
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    # Auto scale Qt apps
    environment.sessionVariables.QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    environment.sessionVariables.QT_ENABLE_HIGHDPI_SCALING = "1";

    # Define a user account. Don't forget to set a password with ‘passwd’.
    programs.zsh.enable = true;
    users.users.epetousis = {
      description = "Evangelos Petousis";
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable ‘sudo’ for the user.
        "networkmanager" # Allow editing network configurations without requiring sudo.
      ];
      packages = with pkgs; [
        (chromium.override {
          commandLineArgs = [
            "--enable-features=TouchpadOverscrollHistoryNavigation"
          ];
        })
      ];
      shell = pkgs.zsh;
    };

    home-manager.users.epetousis = import ./home.nix;

    environment.systemPackages = with pkgs; [
      emacs-lsp-booster
      ffmpeg
      fd
      fira-code
      fira-code-nerdfont
      fzf
      hunspell
      hunspellDicts.en_AU
      jq
      mosh
      mpv
      nil
      nix-output-monitor
      rclone
      ripgrep
      source-sans-pro
      source-han-sans
      tmux
      wl-clipboard
    ];

    # Enable Plymouth to replace boot text.
    boot.plymouth.enable = true;

    # Disable nano (which is normally included by default).
    programs.nano.enable = false;

    services.emacs = {
      enable = true;
      package = pkgs.emacsPackages.evansEmacs;
      defaultEditor = true;
    };

  };
}
