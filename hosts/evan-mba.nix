{ config, pkgs, lib, ... }:

{
  imports = [
    ../modules/symlink-mac-apps.nix
    ../modules/bitlbee.nix
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      pkgs.cachix
      pkgs.pngpaste # Required to paste images into telega.el
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
    };
    taps = map (key: builtins.replaceStrings ["homebrew-"] [""] key) (builtins.attrNames config.nix-homebrew.taps);
    casks = [
      "bettertouchtool"
      "bitwarden"
      "bluos-controller"
      "cryptomator"
      "cyberduck"
      "discord"
      "displaylink"
      "elgato-stream-deck"
      "font-fira-code"
      "font-fira-code-nerd-font"
      "gpg-suite-no-mail"
      "google-chrome"
      "iina"
      "istat-menus"
      "iterm2-beta"
      "karabiner-elements"
      "keka"
      "lasso"
      "macfuse"
      "monitorcontrol"
      "obs"
      "parsec"
      "raycast"
      "rekordbox"
      "sf-symbols"
      "signal"
      "soundsource"
      "spotify"
      "syncthing"
      "thingsmacsandboxhelper"
      "transmission"
      "unnaturalscrollwheels"
      "utm"
      "visual-studio-code"
      "yubico-yubikey-manager"
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
    };
    extraConfig = ''
      yabai -m rule --add label="Firefox PIP" app="^Firefox$" title="^(Picture-in-Picture)$" manage=off
    '';
  };

  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
    # mode switching
    ctrl + alt - e : yabai -m space --layout bsp
    ctrl + alt - s : yabai -m space --layout stack

    :: passthrough
    shift + alt - d ; passthrough
    passthrough < shift + alt - d ; default

    # Based off of https://gist.github.com/ethan-leba/760054f36a2f7c144c6b06ab6458fae6
    # move focus when not in emacs (emacs has its own controls)
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

    # move focus to display
    ctrl + alt - i : yabai -m display --focus prev
    ctrl + alt - o : yabai -m display --focus next

    # move focused window
    ctrl + alt - j : yabai -m window --warp stack.next || yabai -m window --warp south
    ctrl + alt - k : yabai -m window --warp stack.prev || yabai -m window --warp north
    ctrl + alt - h : yabai -m window --warp west
    ctrl + alt - l : yabai -m window --warp east

    # move focused window to monitor
    alt - q : yabai -m window --display prev
    alt - w : yabai -m window --display next

    # move focused window to space
    alt - e : yabai -m window --space prev
    alt - r : yabai -m window --space next

    # switch spaces
    alt - n : yabai -m space --focus prev
    alt - m : yabai -m space --focus next

    # resize active window outwards
    alt - u : yabai -m window --resize left:-20:0
    alt - i : yabai -m window --resize bottom:0:20
    alt - o : yabai -m window --resize top:0:-20
    alt - p : yabai -m window --resize right:20:0

    # resize active window inwards
    shift + alt - u : yabai -m window --resize left:20:0
    shift + alt - i : yabai -m window --resize bottom:0:-20
    shift + alt - o : yabai -m window --resize top:0:20
    shift + alt - p : yabai -m window --resize right:-20:0

    # full screen
    shift + alt - x : yabai -m window --toggle zoom-parent
    shift + alt - c : yabai -m window --toggle zoom-fullscreen

    # Balance out all windows both horizontally and vertically to occupy the same space
    alt - b : yabai -m space --balance

    # Flip the tree horizontally
    alt - c : yabai -m space --mirror x-axis

    # Flip the tree vertically
    alt - v : yabai -m space --mirror y-axis

    # Rotate tree 90 degrees
    alt - g : yabai -m space --rotate 90

    # Toggle window split between vertical and horizontal
    alt - s : yabai -m window --toggle split
    '';
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
