{ config, pkgs, lib, ... }:

{
  imports = [
    ../../modules/symlink-mac-apps.nix
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      pkgs.pngpaste # Required to paste images into telega.el
      pkgs.emacsPackages.evansEmacs
    ];

  system.symlinkApps.enable = true;

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  networking.hostName = "evan-mba-macos";
  # Use U+2019 apostrophe due to string not being escaped by nix-darwin
  networking.computerName = "Evan’s MacBook Air";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.settings = {
    extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
    experimental-features = [ "nix-command" "flakes" ];
    sandbox = true;
    trusted-users = [
      "epetousis"
    ];
  };

  # Enable nix-darwin's Linux builder.
  nix.linux-builder.enable = true;

  users.users.epetousis = {
    name = "epetousis";
    home = "/Users/epetousis";
  };

  home-manager.users.epetousis.imports = [
    ../../modules/home.nix
  ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  nixpkgs.overlays = [
    (import ../../overlays)
  ];

  nixpkgs.config.allowUnfree = true;

  services.emacs = {
    enable = true;
    package = pkgs.emacsPackages.evansEmacs;
    # HACK: traverse out of bin/ to open the Emacs app binary instead.
    # This means that the Emacs daemon will use the correct name and icon.
    exec = "../Applications/Emacs.app/Contents/MacOS/Emacs";
  };

  # Use 1Password agent for SSH.
  home-manager.users.epetousis.programs.ssh.matchBlocks."*".extraOptions.IdentityAgent = pkgs.lib.mkForce "\"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";

  # Use 1Password for Git commit signing.
  home-manager.users.epetousis.programs.git.iniContent."gpg \"ssh\"".program = pkgs.lib.mkForce "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
