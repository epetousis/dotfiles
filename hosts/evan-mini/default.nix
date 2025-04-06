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

  networking.hostName = "evan-mini";
  # Use U+2019 apostrophe due to string not being escaped by nix-darwin
  networking.computerName = "Evan’s Mac mini";

  nix.package = pkgs.lix;

  nix.settings = {
    extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
    experimental-features = [ "nix-command" "flakes" ];
    sandbox = true;
    trusted-users = [
      "epetousis"
    ];
  };

  # Enable nix-darwin's Linux builder.
  # Disabled until https://github.com/nix-darwin/nix-darwin/issues/1081 is resolved.
  nix.linux-builder.enable = false;

  # Make nix-darwin manage my user. The docs say not to add the admin user to this, but Michael says it's fine! https://github.com/nix-darwin/nix-darwin/issues/1237#issuecomment-2562247579
  users.knownUsers = [ "epetousis" ];
  users.users.epetousis = {
    name = "epetousis";
    home = "/Users/epetousis";
    shell = pkgs.fish;
    uid = 501; # The initial macOS admin user should have a UID of 501.
  };

  home-manager.users.epetousis.imports = [
    ../../modules/home.nix
  ];

  # Lix installs with this nixbld identifier, so define it for nix-darwin
  ids.gids.nixbld = 350;

  programs.fish = {
    enable = true;
    shellInit = ''
    direnv hook fish | source
    '';
  };

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
