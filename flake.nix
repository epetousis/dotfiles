{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Install Disko for disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
    nixpkgs-firefox-darwin.inputs.nixpkgs.follows = "nixpkgs";

    nil.url = "github:oxalica/nil";

    nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";
    asahi-firmware = {
      url = "git+ssh://git@github.com/epetousis/asahi-firmware";
      flake = false;
    };

    nixos-aarch64-widevine.url = "github:epetousis/nixos-aarch64-widevine";

    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-cask-versions = {
      url = "github:homebrew/homebrew-cask-versions";
      flake = false;
    };
    homebrew-cask-fonts = {
      url = "github:homebrew/homebrew-cask-fonts";
      flake = false;
    };
    homebrew-cask-drivers = {
      url = "github:homebrew/homebrew-cask-drivers";
      flake = false;
    };
  };

  nixConfig = {
    extra-substituters = [
      # If you update this, update the nix-defaults variable in the output.
      "https://nix-community.cachix.org"
      "https://cachix.org/api/v1/cache/emacs"
      "https://epetousis.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
    ];
  };

  outputs = {
    self,
      darwin,
      nixpkgs,
      nixpkgs-stable,
      home-manager,
      disko,
      emacs-lsp-booster,
      nix-homebrew,
      nixos-apple-silicon,
      homebrew-core,
      homebrew-cask,
      homebrew-bundle,
      homebrew-cask-versions,
      homebrew-cask-fonts,
      homebrew-cask-drivers,
      ...
  }@inputs:
  let
    nixpkgs-defaults = {
      nixpkgs.overlays = [
        emacs-lsp-booster.overlays.default
        inputs.nil.overlays.nil
        (import ./overlays)
      ];
      nixpkgs.config.allowUnfree = true;
    };

    nix-defaults = {
      home-manager.useGlobalPkgs = true;
      nix.settings.trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cachix.org/api/v1/cache/emacs"
        "https://epetousis.cachix.org"
      ];
      nix.settings.trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
      ];
      nix.settings.experimental-features = [ "nix-command" "flakes" ];
    } // nixpkgs-defaults;
  in {
    darwinConfigurations."evan-mba" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        nix-defaults
        home-manager.darwinModules.home-manager
        ./hosts/evan-mba.nix
        nix-homebrew.darwinModules.nix-homebrew
        {
          nix-homebrew.taps = {
            "homebrew/homebrew-core" = homebrew-core;
            "homebrew/homebrew-cask" = homebrew-cask;
            "homebrew/homebrew-bundle" = homebrew-bundle;
            "homebrew/homebrew-cask-versions" = homebrew-cask-versions;
            "homebrew/homebrew-cask-fonts" = homebrew-cask-fonts;
            "homebrew/homebrew-cask-drivers" = homebrew-cask-drivers;
          };
          nix-homebrew.mutableTaps = false;
          nixpkgs.overlays = [ inputs.nixpkgs-firefox-darwin.overlay ];
        }
      ];
    };

    nixosConfigurations."evan-mba" = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        nix-defaults
        ./hosts/evan-mba-nix
        home-manager.nixosModules.home-manager
        nixos-apple-silicon.nixosModules.default
        {
          hardware.asahi.peripheralFirmwareDirectory = inputs.asahi-firmware;
          nixpkgs.overlays = [ inputs.nixos-aarch64-widevine.overlays.default ];
        }
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
