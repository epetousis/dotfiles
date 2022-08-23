{
  description = "Evan's system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = github:nix-community/home-manager/release-22.05;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager }: {
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
  };
}