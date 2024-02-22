# Dotfiles

My dotfiles.

Please feel free to use my overlays, modules, or any part of my dotfiles you feel you need. If you'd like to modify and submit any of it to nixpkgs, you are free to do so.

## How to install

###### macOS Host

1. [Install Nix.](https://nixos.org/download.html)
2. Clone this repo to `~/.local/share/dotfiles`.
3. Run `nix run nix-darwin -- switch --flake ~/.local/share/dotfiles`.

After nix-darwin has finished installing, use `darwin-rebuild switch --flake ~/.local/share/.dotfiles` to build new system generations in future.

###### NixOS Host

1. [Install NixOS.](https://nixos.org/download.html)
2. Clone this repo to `~/.local/share/dotfiles`.
3. Run `sudo nixos-rebuild switch --flake ~/.local/share/.dotfiles`.

##### Standalone Host

1. [Install Nix.]()
2. Clone this repo to your location of choice.
3. Inside the dotfiles directory, run `nix --extra-experimental-features "nix-command flakes" run home-manager/master -- switch --flake . --extra-experimental-features "nix-command flakes"`.

You can now manage building and switching generations using the `home-manager` command.

## Optional Dependencies

While not required to use these dotfiles, the zsh config provides support for these tools.

- [Powerlevel10k](https://github.com/romkatv/powerlevel10k#installation) - the manual and Homebrew installations are supported.
