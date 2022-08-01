# Dotfiles

My dotfiles. My primary shell is zsh, and so these dotfiles currently only support zsh (where applicable).

## How to install

###### home-manager

1. [Install Nix.](https://nixos.org/download.html)
2. Install home-manager by adding it to your `environment.systemPackages` in configuration.nix, or by running `nix-env -iA nixpkgs.home-manager` (`nix-env -iA nixos.home-manager` on NixOS).
3. Clone this repo to `~/.dotfiles`.
4. Run `cd ~/.dotfiles && setopt extended_glob && stow ^README.md`.
5. Run `home-manager switch`.

You'll also need to [install vim-plug](https://github.com/junegunn/vim-plug) before opening nvim.

###### Stow

1. Install GNU stow.
2. Clone this repo to `~/.dotfiles`.
3. Run `cd ~/.dotfiles && setopt extended_glob && stow ^README.md`.

You'll also need to [install vim-plug](https://github.com/junegunn/vim-plug) before opening nvim.

## Optional Dependencies

While not required to use these dotfiles, the zsh config provides support for these tools.

- [Powerlevel10k](https://github.com/romkatv/powerlevel10k#installation) - the manual and Homebrew installations are supported.
- [Pyenv](https://github.com/pyenv/pyenv-installer#installation--update--uninstallation)
- [fnm](https://github.com/Schniz/fnm#installation) and `npm install -g yarn` after installing a Node version
- [Rust](https://www.rust-lang.org/tools/install)
- [keychain](https://www.funtoo.org/Keychain) - not recommended on macOS as the provided ssh config will use the macOS Keychain

