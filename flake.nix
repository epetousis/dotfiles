{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    # Install Disko for disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-stable, home-manager, emacs-overlay, disko }:
  let
    nixpkgs-defaults = {
      nixpkgs.overlays = [
        emacs-overlay.overlays.package
      ];
    };

    nix-defaults = {
      home-manager.useGlobalPkgs = true;
    } // nixpkgs-defaults;
  in {
    nixConfig = {
      extra-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
        "https://cachix.org/api/v1/cache/emacs"
        "https://epetousis.cachix.org"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
      ];
    };

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
        ./hosts/evan-pc
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
      ];
    };

    nixosConfigurations.raspberry = nixpkgs-stable.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        ./hosts/raspberry
      ];
    };

    homeConfigurations."epetousis" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        nixpkgs-defaults
        ./modules/home.nix
        {
          home.username = "epetousis";
          home.homeDirectory = "/home/epetousis";
        }
      ];
    };

    homeConfigurations."epetousis@aarch64" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      modules = [
        nixpkgs-defaults
        ./modules/home.nix
        {
          home.username = "epetousis";
          home.homeDirectory = "/home/epetousis";
        }
      ];
    };
  };
}
