# Dotfiles

My dotfiles.

Please feel free to use my overlays, modules, or any part of my dotfiles you feel you need. If you'd like to modify and submit any of it to nixpkgs, you are free to do so.

## How to install

###### macOS Host

1. [Install Lix.](https://lix.systems/install)
2. Clone this repo to `~/.local/share/dotfiles`.
3. Run `sudo nix run nix-darwin -- switch --flake ~/.local/share/dotfiles`.

After nix-darwin has finished installing, use `sudo darwin-rebuild switch --flake ~/.local/share/dotfiles` to build new system generations in future.

###### NixOS Host

1. [Download and boot the NixOS installer.](https://nixos.org/download.html)
2. Enter a root shell with `sudo -i`.
3. If required, run `nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko --flake github:epetousis/dotfiles/main#[device-name]` to partition and automount all necessary filesystems. Otherwise, mount a root and EFI partition to `/mnt` and `/mnt/boot` respectively.
> [!TIP]
> You'll know if you forgot this step if you run out of disk space during the install process, as nixos-install will attempt to install to the non-existent mountpoint and fill up the installer's tmpfs instead.
4. Run `nixos-install --flake github:epetousis/dotfiles/main#[device-name]` to install to the mounted partitions.

After installing, use `sudo nixos-rebuild switch --flake ~/.local/share/.dotfiles` to build new system generations in future.

Note that you can also switch to this config after installing NixOS, or even use `sudo touch /etc/NIXOS && sudo touch /etc/NIXOS_LUSTRATE && sudo mv -v /boot /boot.bak && sudo /nix/var/nix/profiles/system/bin/switch-to-configuration boot` to overwrite your existing Linux install. See https://nixos.org/manual/nixos/stable/#sec-installing-from-other-distro for more info.

##### Standalone Host

1. [Install Nix.]()
2. Clone this repo to your location of choice.
3. Inside the dotfiles directory, run `nix --extra-experimental-features "nix-command flakes" run home-manager/master -- switch --flake . --extra-experimental-features "nix-command flakes"`.

You can now manage building and switching generations using the `home-manager` command.
