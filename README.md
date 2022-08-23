# Dotfiles

My dotfiles. My primary shell is zsh, and so these dotfiles currently only support zsh (where applicable).

## How to install

Note that with any method of installation, you'll also need to [install vim-plug](https://github.com/junegunn/vim-plug) before opening nvim.

###### macOS Host

1. [Install Nix.](https://nixos.org/download.html)
2. [Install nix-darwin.](https://github.com/LnL7/nix-darwin#install)
3. Clone this repo to `~/.dotfiles`.
4. Run `cd ~/.dotfiles && nix build .\#darwinConfigurations.<hostname>.system`.
5. Run `./result/sw/bin/darwin-rebuild switch --flake .`.

###### NixOS Host

1. [Install NixOS.](https://nixos.org/download.html)
2. Clone this repo to `~/.dotfiles`.
3. Run `cd ~/.dotfiles && sudo nixos-rebuild switch --flake .`.

## Optional Dependencies

While not required to use these dotfiles, the zsh config provides support for these tools.

- [Powerlevel10k](https://github.com/romkatv/powerlevel10k#installation) - the manual and Homebrew installations are supported.
