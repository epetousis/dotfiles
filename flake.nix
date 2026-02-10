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

    # Install Disko for disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs-nixos-unstable";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Use a pregenerated nix-index database instead of generating on device.
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs-unstable";
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
      nixpkgs-unstable,
      nixpkgs-nixos-unstable,
      nixpkgs-nixos-stable,
      home-manager,
      disko,
      emacs-lsp-booster,
      nix-index-database,
      ...
  }@inputs:
  let
    nixpkgs-defaults = {
      nixpkgs.overlays = [
        emacs-lsp-booster.overlays.default
        inputs.emacs-overlay.overlays.package
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

    overlays = {
      default = import ./overlays;
      emacs = import ./overlays/emacs/overlay.nix;
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

    nixosConfigurations."evan-pc" = nixpkgs-nixos-unstable.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nix-defaults
        ./hosts/evan-pc
        home-manager.nixosModules.home-manager
        home-manager-defaults
        disko.nixosModules.disko
        nix-index-database.nixosModules.nix-index
      ];
    };

    nixosConfigurations.raspberry = nixpkgs-nixos-stable.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        ./hosts/raspberry
        nix-defaults
      ];
    };

    homeConfigurations."epetousis" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs-unstable.legacyPackages.x86_64-linux;
      modules = [
        nixpkgs-defaults
        ./modules/home.nix
        home-manager-defaults
        {
          home.username = "epetousis";
          home.homeDirectory = "/home/epetousis";
        }
      ];
    };

    homeConfigurations."epetousis@aarch64" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs-unstable.legacyPackages.aarch64-linux;
      modules = [
        nixpkgs-defaults
        ./modules/home.nix
        home-manager-defaults
        {
          home.username = "epetousis";
          home.homeDirectory = "/home/epetousis";
        }
      ];
    };
  };
}
