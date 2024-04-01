{ config, pkgs, lib, ... }:

{
  imports = [
    ../modules/symlink-mac-apps.nix
    ../modules/bitlbee.nix
    ../modules/sketchybar.nix
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      pkgs.cachix
      pkgs.pngpaste # Required to paste images into telega.el
      pkgs.weechat
      pkgs.bluos-controller
      pkgs.slack
    ];

  system.symlinkApps.enable = true;

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  networking.hostName = "evan-mba";
  # Use U+2019 apostrophe due to string not being escaped by nix-darwin
  networking.computerName = "Evan’s MacBook Air";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nixUnstable;

  nix.settings = {
    extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
    experimental-features = [ "nix-command" "flakes" ];
    sandbox = true;
  };

  users.users.epetousis = {
    name = "epetousis";
    home = "/Users/epetousis";
  };

  home-manager.users.epetousis.imports = [
    ../modules/home.nix
  ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Ensure Homebrew is installed
  nix-homebrew = {
    enable = true;
    enableRosetta = true;
    user = "epetousis";
    autoMigrate = true;
  };

  homebrew = {
    enable = true;
    onActivation = {
      upgrade = true;
      cleanup = "uninstall";
    };
    taps = map (key: builtins.replaceStrings ["homebrew-"] [""] key) (builtins.attrNames config.nix-homebrew.taps);
    casks = [
      "bitwarden"
      "cryptomator"
      "displaylink"
      "elgato-stream-deck"
      "google-chrome"
      "macfuse"
      "obs"
      "raycast"
      "rekordbox"
      "signal"
      "thingsmacsandboxhelper"
      "zoom"
    ];
  };

  # Speed up Dock autohide
  system.defaults.dock = {
    autohide = true;
    autohide-delay = 0.4;
    autohide-time-modifier = 0.15;
    mineffect = "scale";
    show-recents = false;
    mru-spaces = false;
    # Disable the default quick note hot corner
    wvous-br-corner = 1;
  };

  nixpkgs.overlays = [
    (import ../overlays)
  ];

  nixpkgs.config.allowUnfree = true;

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
    package = pkgs.yabai;
    config = {
      layout = "bsp";
      window_placement    = "second_child";
      window_opacity      = "off";
      top_padding         = 10;
      bottom_padding      = 10;
      left_padding        = 10;
      right_padding       = 10;
      window_gap          = 10;
      external_bar = lib.optionals config.services.sketchybar.enable "all:40:0";
    };
    extraConfig = ''
      yabai -m rule --add label="Firefox PIP" app="^Firefox$" title="^(Picture-in-Picture)$" manage=off
    '';
  };

  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
    # Define a passthrough mode to temp disable shortcuts in
    :: passthrough
    shift + alt - d ; passthrough
    passthrough < shift + alt - d ; default

    ## Focus switching
    # Based off of https://gist.github.com/ethan-leba/760054f36a2f7c144c6b06ab6458fae6
    # Switch focus to window (except in emacs - it has its own controls)
    alt - h [
      *      : yabai -m window --focus west
      "Emacs" ~
    ]
    alt - j  [
      *      : yabai -m window --focus stack.next || yabai -m window --focus south
      "Emacs" ~
    ]
    alt - k  [
      *      : yabai -m window --focus stack.prev || yabai -m window --focus north
      "Emacs" ~
    ]
    alt - l  [
      *      : yabai -m window --focus east
      "Emacs" ~
    ]

    # Switch focus to monitor
    alt - q : yabai -m display --focus prev
    alt - w : yabai -m display --focus next

    # Switch focus to space
    alt - e : yabai -m space --focus prev
    alt - r : yabai -m space --focus next

    ## Window movement
    # move focused window
    shift + alt - j : yabai -m window --warp stack.next || yabai -m window --warp south
    shift + alt - k : yabai -m window --warp stack.prev || yabai -m window --warp north
    shift + alt - h : yabai -m window --warp west
    shift + alt - l : yabai -m window --warp east

    # Move focused window to monitor
    shift + alt - q : yabai -m window --display prev
    shift + alt - w : yabai -m window --display next

    # Move focused window to space
    shift + alt - e : yabai -m window --space prev
    shift + alt - r : yabai -m window --space next

    ## Extra window movement commands
    # Queue insert on focused window
    # (an overlay will display indicating the insert position of the next moved/opened window)
    ctrl + alt - j : yabai -m window --insert south
    ctrl + alt - k : yabai -m window --insert north
    ctrl + alt - h : yabai -m window --insert west
    ctrl + alt - l : yabai -m window --insert east

    # Swap focused window
    shift + ctrl + alt - j : yabai -m window --swap south
    shift + ctrl + alt - k : yabai -m window --swap north
    shift + ctrl + alt - h : yabai -m window --swap west
    shift + ctrl + alt - l : yabai -m window --swap east

    ## Resizing
    # Resize active window edges outwards
    alt - u : yabai -m window --resize left:-20:0
    alt - i : yabai -m window --resize bottom:0:20
    alt - o : yabai -m window --resize top:0:-20
    alt - p : yabai -m window --resize right:20:0

    # Resize active window edges inwards
    shift + alt - u : yabai -m window --resize left:20:0
    shift + alt - i : yabai -m window --resize bottom:0:-20
    shift + alt - o : yabai -m window --resize top:0:20
    shift + alt - p : yabai -m window --resize right:-20:0

    # Full screen
    shift + alt - x : yabai -m window --toggle zoom-parent
    shift + alt - c : yabai -m window --toggle zoom-fullscreen

    ## Layout
    # Mode switching
    ctrl + alt - x : yabai -m space --layout bsp
    ctrl + alt - c : yabai -m space --layout stack

    # Balance out all windows both horizontally and vertically to occupy the same space
    shift + alt - b : yabai -m space --balance

    # Flip the tree vertically
    alt - v : yabai -m space --mirror y-axis

    # Flip the tree horizontally
    shift + alt - v : yabai -m space --mirror x-axis

    # Rotate tree 90 degrees
    alt - g : yabai -m space --rotate 90

    # Toggle window split between vertical and horizontal
    alt - s : yabai -m window --toggle split

    # Create space
    ctrl + alt - n : yabai -m space --create
    ctrl + alt - 0x2F : yabai -m space --destroy # 0x2F = .
    '';
  };

  services.evanSketchybar.enable = true;

  system.defaults.finder.CreateDesktop = false;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
