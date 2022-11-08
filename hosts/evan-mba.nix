{ config, pkgs, lib, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  networking.hostName = "evan-mba";
  # Use U+2019 apostrophe due to string not being escaped by nix-darwin
  networking.computerName = "Evan’s MacBook Air";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

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

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
      upgrade = true;
    };
    taps = [
      "homebrew/cask"
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "homebrew/cask-drivers"
    ];
    casks = [
      "1password"
      "altserver"
      "anki"
      "balenaetcher"
      "barrier"
      "battle-net"
      "betterdummy"
      "chatterino"
      "coconutbattery"
      "cryptomator"
      "cyberduck"
      "discord"
      "displaylink"
      "displaylink-login-extension"
      "dosbox-x"
      "element"
      "firefox"
      "font-fira-code"
      "font-fira-code-nerd-font"
      "google-chrome"
      "graphiql"
      "hot"
      "iina"
      "istat-menus"
      "iterm2-beta"
      "keepingyouawake"
      "maccy"
      "macfuse"
      "mailtrackerblocker"
      "minecraft"
      "mirrorop"
      "mixxx"
      "monitorcontrol"
      "ngrok"
      "notion"
      "obs"
      "openemu"
      "parsec"
      "paw"
      "plex"
      "protonvpn"
      "pycharm"
      "rekordbox"
      "runelite"
      "sf-symbols"
      "signal"
      "soulseek"
      "soundsource"
      "spotify"
      "sublime-text"
      "syncthing"
      "the-unarchiver"
      "touchosc-bridge"
      "transmission"
      "utm"
      "vimr"
      "visual-studio-code"
      "webstorm"
      "yubico-yubikey-manager"
      "zoom"
    ];
  };

  # Speed up Dock autohide
  system.defaults.dock = {
    autohide = true;
    autohide-delay = "0.4";
    autohide-time-modifier = "0.15";
    mineffect = "scale";
    show-recents = false;
    mru-spaces = false;
    # Disable the default quick note hot corner
    wvous-br-corner = 1;
  };

  nixpkgs.overlays = [
    (import ../overlays)
  ];

  services.yabai = {
    enable = false;
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
    enable = false;
    package = pkgs.skhd;
    skhdConfig = ''
    # mode switching
    ctrl + alt - e : yabai -m space --layout bsp
    ctrl + alt - s : yabai -m space --layout stack

    # move focus
    ctrl + alt - j : yabai -m window --focus stack.next || yabai -m window --focus south
    ctrl + alt - k : yabai -m window --focus stack.prev || yabai -m window --focus north
    ctrl + alt - h : yabai -m window --focus west
    ctrl + alt - l : yabai -m window --focus east

    # move focus to display
    ctrl + alt - i : yabai -m display --focus prev
    ctrl + alt - o : yabai -m display --focus next

    # move focused window
    ctrl + alt + cmd - j : yabai -m window --warp stack.next || yabai -m window --warp south
    ctrl + alt + cmd - k : yabai -m window --warp stack.prev || yabai -m window --warp north
    ctrl + alt + cmd - h : yabai -m window --warp west
    ctrl + alt + cmd - l : yabai -m window --warp east

    # move focused window to monitor
    ctrl + alt + cmd - i : yabai -m window --display prev
    ctrl + alt + cmd - o : yabai -m window --display next

    # move focused window to space
    ctrl + alt + shift - i : yabai -m window --space prev
    ctrl + alt + shift - o : yabai -m window --space next

    # resize active window outwards
    shift + alt - h : yabai -m window --resize left:-20:0
    shift + alt - j : yabai -m window --resize bottom:0:20
    shift + alt - k : yabai -m window --resize top:0:-20
    shift + alt - l : yabai -m window --resize right:20:0

    # resize active window inwards
    shift + alt + ctrl - h : yabai -m window --resize left:20:0
    shift + alt + ctrl - j : yabai -m window --resize bottom:0:-20
    shift + alt + ctrl - k : yabai -m window --resize top:0:20
    shift + alt + ctrl - l : yabai -m window --resize right:-20:0

    # full screen
    ctrl + alt - f : yabai -m window --toggle zoom-fullscreen
    '';
  };

  # Symlink macOS apps - taken from https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
  system.activationScripts.applications.text = pkgs.lib.mkForce (
    ''
      echo "setting up ~/Applications..." >&2
      rm -rf ~/Applications/Nix\ Apps
      mkdir -p ~/Applications/Nix\ Apps
      for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
        src="$(/usr/bin/stat -f%Y "$app")"
        cp -r "$src" ~/Applications/Nix\ Apps
      done
    ''
  );

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
