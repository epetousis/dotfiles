# Dotfiles

My dotfiles.

Please feel free to use my overlays, modules, or any part of my dotfiles you feel you need. If you'd like to modify and submit any of it to nixpkgs, you are free to do so.

## How to install

###### macOS Host

1. [Install Nix.](https://nixos.org/download.html)
2. [Install nix-darwin.](https://github.com/LnL7/nix-darwin#install)
3. Clone this repo to `~/.dotfiles`.
4. Run `darwin-rebuild switch --flake .dotfiles`.

###### NixOS Host

1. [Install NixOS.](https://nixos.org/download.html)
2. Clone this repo to `~/.dotfiles`.
3. Run `sudo nixos-rebuild switch --flake .dotfiles`.

## Optional Dependencies

While not required to use these dotfiles, the zsh config provides support for these tools.

- [Powerlevel10k](https://github.com/romkatv/powerlevel10k#installation) - the manual and Homebrew installations are supported.
