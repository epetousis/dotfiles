{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/5ad6a14c6bf098e98800b091668718c336effc95";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    lix-module = {
      url = "git+https://git.lix.systems/lix-project/nixos-module";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Install Disko for disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    nil.url = "github:oxalica/nil";

    nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";
    /* asahi-firmware = {
      url = "git+ssh://git@github.com/epetousis/asahi-firmware";
      flake = false;
    }; */

    nixos-aarch64-widevine.url = "github:epetousis/nixos-aarch64-widevine";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-substituters = [
      # If you update this, update the nix-defaults variable in the output.
      "https://nix-community.cachix.org"
      "https://cachix.org/api/v1/cache/emacs"
      "https://epetousis.cachix.org"
      "https://cache.lix.systems"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
      "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
    ];
  };

  outputs = {
    self,
      darwin,
      lix-module,
      nixpkgs,
      nixpkgs-stable,
      home-manager,
      disko,
      emacs-lsp-booster,
      nixos-apple-silicon,
      ...
  }@inputs:
  let
    nixpkgs-defaults = {
      nixpkgs.overlays = [
        emacs-lsp-booster.overlays.default
        inputs.emacs-overlay.overlays.package
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
        "https://cache.lix.systems"
      ];
      nix.settings.trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];
      nix.settings.experimental-features = [ "nix-command" "flakes" ];
    } // nixpkgs-defaults;
  in {
    nixosConfigurations."evan-mba" = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        lix-module.nixosModules.default
        nix-defaults
        ./hosts/evan-mba-nix
        home-manager.nixosModules.home-manager
        nixos-apple-silicon.nixosModules.default
        {
          /* hardware.asahi.peripheralFirmwareDirectory = inputs.asahi-firmware; */
          nixpkgs.overlays = [ inputs.nixos-aarch64-widevine.overlays.default ];
        }
      ];
    };

    darwinConfigurations."evan-mba-macos" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        lix-module.nixosModules.default
        nix-defaults
        home-manager.darwinModules.home-manager
        ./hosts/evan-mba.nix
      ];
    };

    nixosConfigurations."evan-pc" = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules = [
        lix-module.nixosModules.default
        nix-defaults
        ./hosts/evan-pc
        home-manager.nixosModules.home-manager
        disko.nixosModules.disko
        {
          nixpkgs.overlays = [
            (f: p: {
              kdePackages = p.kdePackages // {
                kwin = (import nixpkgs-stable {
                  inherit system;
                }).kdePackages.kwin;
              };
            })
          ];
        }
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
