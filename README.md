# Dotfiles

## How to install

1. Install GNU stow.
2. Clone this repo to `~/.dotfiles`.
3. Run `cd ~/.dotfiles && setopt extended_glob && stow ^README.md`.

You'll also need to [install vim-plug](https://github.com/junegunn/vim-plug) before opening nvim.

You'll probably want to [install Powerlevel10k](https://github.com/romkatv/powerlevel10k#installation). The manual and Homebrew installations are supported.

## Optional Dependencies

While not required to use these dotfiles, the zsh config provides support for these tools.

- [Pyenv](https://github.com/pyenv/pyenv-installer#installation--update--uninstallation)
- [fnm](https://github.com/Schniz/fnm#installation) and `npm install -g yarn` after installing a Node version
- [Rust](https://www.rust-lang.org/tools/install)

