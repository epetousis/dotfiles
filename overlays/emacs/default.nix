{ pkgs, lib, ... }:

let
  # Add emacs-plus patches to nix emacs
  emacsPlus = pkgs.emacs.overrideAttrs(f: p: let
    patchCommit = "d565d8ada1c2e00c3182b57ea6bc6497ebb7bdef";
  in {
    patches = p.patches
              ++ lib.optionals pkgs.stdenv.isDarwin [
                # Fix OS window role (surely this should have been upstreamed by now...)
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${patchCommit}/patches/emacs-28/fix-window-role.patch";
                  sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
                })
                # Use poll instead of select to get file descriptors
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${patchCommit}/patches/emacs-29/poll.patch";
                  sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
                })
                # Enable rounded window with no decoration
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${patchCommit}/patches/emacs-29/round-undecorated-frame.patch";
                  sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
                })
                # Make Emacs aware of OS-level light/dark mode
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${patchCommit}/patches/emacs-28/system-appearance.patch";
                  sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
                })
                # Add no-frame-refocus patch from emacs-plus
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/${patchCommit}/patches/emacs-28/no-frame-refocus-cocoa.patch";
                  sha256 = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
                })
              ];
  });
  # Customise the app bundle with a new icon
  emacsPlusWithIcon = pkgs.callPackage ../../modules/icon-override.nix {
    pkg = emacsPlus;
    iconPath = ./elrumo2.icns;
  };
  # On Linux, we use the pure GTK build to enable full Wayland support.
  appropriateEmacs = if pkgs.stdenv.hostPlatform.isDarwin then emacsPlusWithIcon else pkgs.emacs29-pgtk;
  # Finally, provide our customised emacs with our preferred packages.
  # (This should come as late as possible in the process.)
  evansEmacs = (pkgs.emacsPackagesFor appropriateEmacs).emacsWithPackages (epkgs: [
    # emacs packages
    epkgs.company
    epkgs.dtrt-indent
    epkgs.editorconfig
    epkgs.eglot
    epkgs.envrc
    epkgs.magit
    epkgs.doom-themes
    epkgs.melpaPackages.dired-sidebar
    epkgs.avy

    # Modes
    epkgs.nix-mode
    epkgs.rust-mode
    epkgs.typescript-mode
    epkgs.web-mode
    epkgs.melpaPackages.telega
    epkgs.melpaPackages.php-mode
    (epkgs.callPackage ../eglot-booster.nix {})
    epkgs.vterm
    epkgs.ement
    epkgs.mu4e
    epkgs.elfeed
  ]);
in

evansEmacs
