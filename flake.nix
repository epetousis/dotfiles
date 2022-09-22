{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.url = github:nix-community/nixos-wsl/main;
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    emacs.url = github:nix-community/emacs-overlay/master;
    emacs.inputs.nixpkgs.follows = "nixpkgs";
    emacs-mac.url = github:cmacrae/emacs;
    emacs-mac.inputs.nixpkgs.follows = "nixpkgs";

    # This is a specific revision of nixpkgs with a cross-compiled stdenv.
    # Without this, you'll likely see random linker errors.
    # See https://github.com/tpwrules/nixos-m1/blob/main/nix/pins.nix
    nixpkgs-cross-stdenv.url = "github:nixos/nixpkgs/f677051b8dc0b5e2a9348941c99eea8c4b0ff28f";
    m1-support.url = github:tpwrules/nixos-m1/main;
    m1-support.flake = false;
    m1-firmware.url = "/etc/nixos/m1-support";
    m1-firmware.flake = false;
  };

  outputs = { self, darwin, nixpkgs, nixpkgs-stable, home-manager, nixos-wsl, emacs, emacs-mac, m1-support, m1-firmware, nixpkgs-cross-stdenv }:
  let
    nix-defaults = {
      home-manager.useGlobalPkgs = true;

      nix = {
        settings = {
          substituters = [
            "https://nix-community.cachix.org"
            "https://cache.nixos.org/"
            "https://cachix.org/api/v1/cache/emacs"
          ];
          trusted-public-keys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
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

    nixosConfigurations."evan-mba-nix" = nixpkgs-cross-stdenv.lib.nixosSystem rec {
      system = "aarch64-linux";
      modules = [
        nix-defaults
        home-manager.nixosModules.home-manager
        # Include the necessary packages and configuration for Apple M1 support.
        # M1 builds currently require a cross-compiled stdenv.
        (m1-support + "/nix/m1-support")
        (m1-firmware + "/firmware")
        ./hosts/evan-mba-nix
      ];
    };

    nixosConfigurations."evan-pc-wsl" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nix-defaults
        nixos-wsl.nixosModules.wsl
        home-manager.nixosModules.home-manager
        ./hosts/evan-pc-wsl.nix
      ];
    };

    nixosConfigurations."evan-pc" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nix-defaults
        home-manager.nixosModules.home-manager
        ./hosts/evan-pc
      ];
    };

    nixosConfigurations.raspberry = nixpkgs-stable.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        ./hosts/raspberry
      ];
    };
  };
}
