{ pkgs, lib, ... }:

let
  # Customise the app bundle with a new icon
  emacsPlusWithIcon = pkgs.callPackage ../../modules/icon-override.nix {
    # Disable native compilation on Darwin for the time being while Nixpkgs issue 395169 is fixed.
    pkg = pkgs.emacs30-pgtk.override { withNativeCompilation = !pkgs.stdenv.hostPlatform.isDarwin; };
    iconPath = ./elrumo2.icns;
  };
  # On Linux, we use the pure GTK build to enable full Wayland support.
  appropriateEmacs = if pkgs.stdenv.hostPlatform.isDarwin then emacsPlusWithIcon else pkgs.emacs30-pgtk;
  # Finally, provide our customised emacs with our preferred packages.
  # (This should come as late as possible in the process.)
  evansEmacs = (pkgs.emacsPackagesFor appropriateEmacs).emacsWithPackages (epkgs: [
    # emacs packages
    epkgs.breadcrumb
    epkgs.company
    epkgs.csv-mode
    epkgs.delight
    epkgs.dired-preview
    epkgs.dtrt-indent
    epkgs.editorconfig
    epkgs.ef-themes
    epkgs.eglot
    epkgs.envrc
    epkgs.evil
    epkgs.fireplace
    epkgs.magit
    epkgs.markdown-mode
    epkgs.melpaPackages.dired-sidebar
    epkgs.melpaPackages.php-mode
    epkgs.nix-mode
    epkgs.ox-slack
    epkgs.rust-mode
    epkgs.treesit-grammars.with-all-grammars
    epkgs.vterm
    epkgs.web-mode
  ]);
in

evansEmacs
