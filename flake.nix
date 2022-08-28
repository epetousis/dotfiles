{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.url = github:nix-community/nixos-wsl/main;
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    emacs.url = github:nix-community/emacs-overlay/master;
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-stable, home-manager, nixos-wsl, emacs }:
  let
    nix-defaults = {
      home-manager.useGlobalPkgs = true;

      nix = {
        settings = {
          substituters = [
            "https://nix-community.cachix.org"
            "https://cache.nixos.org/"
          ];
          trusted-public-keys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
        };
      };

      nixpkgs.overlays = [
        emacs.overlay
      ];
    };
  in {
    # NB: nix-darwin doesn't work with flakes OOB yet.
    # You'll have to install it first through its darwin-installer before building this Flake, and
    # then switch with:
    # $ darwin-rebuild switch --flake .
    darwinConfigurations."evan-mba" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        nix-defaults
        home-manager.darwinModules.home-manager
        ./hosts/evan-mba.nix
      ];
    };

    nixosConfigurations."evan-pc" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nix-defaults
        nixos-wsl.nixosModules.wsl
        home-manager.nixosModules.home-manager
        ./hosts/evan-pc.nix
      ];
    };

    nixosConfigurations.raspberry = nixpkgs-stable.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        nix-defaults
        ./hosts/raspberry
      ];
    };
  };
}