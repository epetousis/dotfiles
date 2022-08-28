{ lib, pkgs, config, modulesPath, ... }:

with lib;
{
  imports = [
    "${modulesPath}/profiles/minimal.nix"
  ];

  wsl = {
    enable = true;
    automountPath = "/mnt";
    defaultUser = "epetousis";
    startMenuLaunchers = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker.enable = true;
  };

  # Define our hostname so switches don't reset it
  networking.hostName = "evan-pc";

  # Enable nix flakes
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  programs.zsh.enable = true;

  users.users.epetousis = {
    isNormalUser = true;
    home = "/home/epetousis";
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };

  home-manager.users.epetousis = import ../modules/home.nix;

  environment.systemPackages = with pkgs; [
    neovim
  ];

  environment.sessionVariables = rec {
    GDK_DPI_SCALE = "1.5";
  };

  fonts = {
    fonts = with pkgs; [
      liberation_ttf
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Liberation Mono" ];
      };
    };
  };

  system.stateVersion = "21.11";
}
