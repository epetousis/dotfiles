{ config, pkgs, lib, ... }:

{
  imports = [
    ../../modules/copy-mac-apps.nix
  ];

  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      pngpaste # Required to paste images into telega.el
      evans-emacs
      ripgrep
      witr
      colima
      docker
      yt-dlp
      ffmpeg
      _1password-cli
      colima
      docker
      nixos-rebuild-ng;
  };

  networking.hostName = "evan-mbp";
  # Use U+2019 apostrophe due to string not being escaped by nix-darwin
  networking.computerName = "Evan’s MacBook Pro";

  nix.package = pkgs.lix;

  nix.settings = {
    extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
    experimental-features = [ "nix-command" "flakes" ];
    sandbox = true;
  };

  # Make nix-darwin manage my user. The docs say not to add the admin user to this, but Michael says it's fine! https://github.com/nix-darwin/nix-darwin/issues/1237#issuecomment-2562247579
  users.knownUsers = [ "epetousis" ];
  users.users.epetousis = {
    name = "epetousis";
    home = "/Users/epetousis";
    uid = 501; # The initial macOS admin user should have a UID of 501.
  };
  system.primaryUser = "epetousis";

  home-manager.users.epetousis.imports = [
    ../../modules/home.nix
  ];

  # Lix installs with this nixbld identifier, so define it for nix-darwin
  ids.gids.nixbld = 350;

  programs.zsh = {
    enable = true;
    promptInit = ''
      if [[ $(${pkgs.procps}/bin/ps -o 'comm=' -p $PPID) != "fish" && -z ''${ZSH_EXECUTION_STRING} && ''${SHLVL} == 1 ]]
      then
        if [[ -o login ]]; then
          LOGIN_OPTION='--login'
        else
          LOGIN_OPTION='''
        fi
        exec fish $LOGIN_OPTION
      fi
    '';
  };

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
    package = pkgs.evans-emacs;
    # HACK: traverse out of bin/ to open the Emacs app binary instead.
    # This means that the Emacs daemon will use the correct name and icon.
    exec = "../Applications/Emacs.app/Contents/MacOS/Emacs";
  };

  # Use 1Password agent for SSH.
  home-manager.users.epetousis.programs.ssh.matchBlocks."*".extraOptions.IdentityAgent = pkgs.lib.mkForce "\"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";

  # Use 1Password for Git commit signing.
  home-manager.users.epetousis.programs.git.iniContent."gpg \"ssh\"".program = pkgs.lib.mkForce "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";

  system = {
    defaults = {
      NSGlobalDomain = {
        # Disable the press and hold accent menu system-wide
        ApplePressAndHoldEnabled = false;
        # Set key repeat to be quick
        KeyRepeat = 2;
        InitialKeyRepeat = 15;
        # Set trackpad speed to a reasonable amount
        "com.apple.trackpad.scaling" = 1.5;
        # Enable tap to click
        "com.apple.mouse.tapBehavior" = 1;
      };
      dock.autohide = true;
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  environment.variables = {
    EDITOR = "emacsclient";
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
