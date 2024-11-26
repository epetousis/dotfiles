{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.symlinkApps;
in {
  options.system.symlinkApps = {
    enable = mkEnableOption "Symlinks applications into the /Applications folder.";

    copiedPackages = mkOption {
      type = types.listOf types.package;
      default = [];
      example = [ "_1password-gui" ];
      description = ''
      A list of packages whose apps should be copied, rather than symlinked.
      This is a last resort option for apps that stubbornly refuse to run outside of /Applications,
      or have other issues e.g code signing related difficulties.
      Use sparingly, as the local store path will not be removed, using disk space unnecessarily.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Copy macOS apps - modified from https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
    # APFS does not support hard links and Finder aliases are a clunky workaround.
    # However, if you symlink the "Contents" folder inside? Both Finder and Dock
    # see the app as a normal one, however all the contents live inside the Nix store.
    # However, `codesign --verify` has issues with this. It is a tradeoff.
    system.activationScripts.applications.text = pkgs.lib.mkForce (
      ''
      # Make sure the shell doesn't split by spaces in the path name
      IFS=$'\n'

      shopt -s nullglob

      echo "symlinking system apps to /Applications..." >&2
      for app in ${config.system.build.applications}/Applications/*.app; do
        src="$(/usr/bin/stat -f%Y "$app")"
        appname="$(/usr/bin/basename "$app")"
        # "
        if [ -n "$appname" ]; then
          rm -rf /Applications/"$appname"
        fi
        mkdir /Applications/"$appname"
        ln -s "$src"/Contents /Applications/"$appname"/Contents
      done

      echo "symlinking home-manager apps to /Applications..." >&2
      for app in ${pkgs.buildEnv {
        name = "applications";
        paths = config.home-manager.users.epetousis.home.packages;
        pathsToLink = "/Applications";
      }}/Applications/*.app; do
        src="$(/usr/bin/stat -f%Y "$app")"
        appname="$(/usr/bin/basename "$app")"
        if [ -n "$appname" ]; then
          rm -rf /Applications/"$appname"
        fi
        mkdir /Applications/"$appname"
        ln -s "$src"/Contents /Applications/"$appname"/Contents
      done

      echo "copying copyPackage apps to /Applications..." >&2
      for app in ${pkgs.buildEnv {
        name = "applications";
        paths = cfg.copiedPackages;
        pathsToLink = "/Applications";
      }}/Applications/*.app; do
        src="$(/usr/bin/stat -f%Y "$app")"
        appname="$(/usr/bin/basename "$app")"
        if [ -n "$appname" ]; then
          rm -rf /Applications/"$appname"
        fi
        cp -Lr "$src" /Applications
      done

      unset IFS
    ''
    );
  };
}
