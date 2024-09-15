{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    # Temporary, see https://github.com/NixOS/nixpkgs/issues/327836#issuecomment-2292084100
    darwin-nixpkgs.url = "github:nixos/nixpkgs?rev=2e92235aa591abc613504fde2546d6f78b18c0cd";

    # Temporary, see https://github.com/NixOS/nixpkgs/pull/338033
    nixpkgs-pr338033.url = "github:nixos/nixpkgs?rev=0c3d12a3e5d4077cf7fee84fcb177237ee8daddf";

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

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-substituters = [
      # If you update this, update the nix-defaults variable in the output.
      "https://nix-community.cachix.org"
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
        "https://epetousis.cachix.org"
        "https://cache.lix.systems"
      ];
      nix.settings.trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "epetousis.cachix.org-1:c87cgNPjvPjqoZX7dbedzBo/cx2ULiGjSNN12VV5bKw="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];
      nix.settings.experimental-features = [ "nix-command" "flakes" ];

      # Make Nix trust admin users
      nix.settings.trusted-users = [
        "root"
        "@wheel"
      ];
    } // nixpkgs-defaults;
  in {
    nixosModules = {
      sharedSettings = import ./modules/shared.nix;
    };

    darwinConfigurations."evan-mba-macos" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        lix-module.nixosModules.default
        nix-defaults
        home-manager.darwinModules.home-manager
        ./hosts/evan-mba.nix
        {
          nixpkgs.overlays = [
            # Temporary, see https://github.com/NixOS/nixpkgs/issues/327836#issuecomment-2292084100
            (final: prev: let
              pkgsDarwin = import inputs.darwin-nixpkgs {inherit (prev) system;};
              pkgsKittyFix = import inputs.nixpkgs-pr338033 { inherit (prev) system; };
            in {
              inherit (pkgsDarwin) swift;
              inherit (pkgsKittyFix) kitty;
              }
            )
          ];
        }
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
        nix-defaults
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
