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
    cleanup = "zap";
    taps = [
      "homebrew/cask"
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "homebrew/cask-drivers"
    ];
    casks = [
      "1password"
      "altserver"
      "amethyst"
      "anki"
      "balenaetcher"
      "barrier"
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
      "emacs"
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
      "rectangle"
      "rekordbox"
      "runelite"
      "sf-symbols"
      "signal"
      "soulseek"
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
