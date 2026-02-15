{
  description = "Evan's system";

  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-nixos-stable.url = "github:nixos/nixpkgs/nixos-24.11";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    my-emacs.url = "github:epetousis/my-emacs";
    my-emacs.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  outputs = {
    self,
      darwin,
      nixpkgs-unstable,
      nixpkgs-nixos-unstable,
      nixpkgs-nixos-stable,
      home-manager,
      my-emacs,
      ...
  }@inputs:
  let
    nixpkgs-defaults = {
      nixpkgs.overlays = [
        my-emacs.overlays.default
        (import ./overlays)
      ];
      nixpkgs.config.allowUnfree = true;
    };

    nix-defaults = {
      nix.settings.trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://epetousis.cachix.org"
      ];
      nix.settings.trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
      ];
      nix.settings.experimental-features = [ "nix-command" "flakes" ];
    } // nixpkgs-defaults;

    home-manager-defaults = {
      home-manager.useGlobalPkgs = true;
    };
  in {
    nixosModules = {
      sharedSettings = import ./modules/shared.nix;
    };

    darwinConfigurations."evan-mbp" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        nix-defaults
        home-manager.darwinModules.home-manager
        home-manager-defaults
        ./hosts/evan-mbp
      ];
    };

    nixosConfigurations.raspberry = nixpkgs-nixos-stable.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        ./hosts/raspberry
        nix-defaults
      ];
    };
  };
}
