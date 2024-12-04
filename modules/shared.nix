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
      fzf
      hunspell
      hunspellDicts.en_AU
      jq
      mosh
      mpv
      nerd-fonts.fira-code
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

    # Remove old generations automatically.
    nix.gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than +15";
    };

    programs.dconf = {
      enable = true;
      profiles.user.databases = [{
        settings = {
          "org/gnome/desktop/wm/keybindings" = {
            close = ["<Super>q" "<Alt>F4"];
            toggle-maximized = ["<Super>m"];
            minimize = ["<Super>comma"];
            move-to-monitor-down = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            move-to-monitor-left = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            move-to-monitor-right = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            move-to-monitor-up = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            switch-to-workspace-down = ["<Primary><Super>Down"];
            switch-to-workspace-up = ["<Primary><Super>Up"];
            switch-to-workspace-left = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            switch-to-workspace-right = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            maximize = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            unmaximize = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
          };

          "org/gnome/shell/keybindings" = {
            show-screenshot-ui = ["<Shift><Super>s"];
            toggle-message-tray = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
          };

          "org/gnome/mutter/keybindings" = {
            # Unbind these, as defaults conflict with focus-left and focus-right
            toggle-tiled-left = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            toggle-tiled-right = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
          };

          "org/gnome/settings-daemon/plugins/media-keys" = {
            screensaver = ["<Super>e"]; # Lock screen shortcut
            home = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            www = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            terminal = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            email = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
          };

          "org/gnome/shell/extensions/pop-shell" = {
            # Disable tiling toggle until https://github.com/pop-os/shell/issues/1733 is fixed
            toggle-tiling = lib.gvariant.mkEmptyArray (lib.gvariant.type.string); # ["<Super>y"];
            toggle-floating = ["<Super>g"];
            toggle-stacking-global = ["<Super>s"];
            tile-enter = ["<Super>Return"];
            tile-accept = ["Return"];
            tile-reject = ["Escape"];
            tile-move-down = ["<Shift><Super>Down" "<Shift><Super>j"];
            tile-move-up = ["<Shift><Super>Up" "<Shift><Super>k"];
            tile-move-left = ["<Shift><Super>Left" "<Shift><Super>h"];
            tile-move-right = ["<Shift><Super>Right" "<Shift><Super>l"];
            pop-monitor-down = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            pop-monitor-up = lib.gvariant.mkEmptyArray (lib.gvariant.type.string);
            pop-workspace-down = ["<Shift><Super>Down" "<Shift><Super>j"];
            pop-workspace-up = ["<Shift><Super>Up" "<Shift><Super>k"];
            pop-monitor-left = ["<Ctrl><Shift><Super>Left" "<Ctrl><Shift><Super>h"];
            pop-monitor-right = ["<Ctrl><Shift><Super>Right" "<Ctrl><Shift><Super>l"];
            focus-left = ["<Super>Left" "<Super>h"];
            focus-down = ["<Super>Down" "<Super>j"];
            focus-up = ["<Super>Up" "<Super>k"];
            focus-right = ["<Super>Right" "<Super>l"];
          };
        };
      }];
    };

  };
}
