{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.url = github:nix-community/nixos-wsl/main;
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, nixos-wsl }: {
    # NB: nix-darwin doesn't work with flakes OOB yet.
    # You'll have to install it first through its darwin-installer before building this Flake, and
    # then switch with:
    # $ darwin-rebuild switch --flake .
    darwinConfigurations."evan-mba" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        home-manager.darwinModules.home-manager
        ./hosts/evan-mba.nix
      ];
    };

    nixosConfigurations."evan-pc" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nixos-wsl.nixosModules.wsl
        home-manager.nixosModules.home-manager
        ./hosts/evan-pc.nix
      ];
    };

    nixosConfigurations.raspberry = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [ ./hosts/raspberry ];
    };
  };
}