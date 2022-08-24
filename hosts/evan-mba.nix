{ config, pkgs, lib, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

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

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
