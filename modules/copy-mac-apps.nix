{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.system.copyApps;
in {
  options.system.copyApps = {
    enable = mkEnableOption "Copy applications into the /Applications folder.";
  };

  config = mkIf cfg.enable {
    # Copy macOS apps - modified from https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
    system.activationScripts.applications.text = pkgs.lib.mkForce (
      ''
      # Make sure the shell doesn't split by spaces in the path name
      IFS=$'\n'

      shopt -s nullglob

      echo "copying apps to /Applications..." >&2
      for app in ${config.system.build.applications}/Applications/*.app; do
        src="$(/usr/bin/stat -f%Y "$app")"
        appname="$(/usr/bin/basename "$app")"
        # "
        if [ -n "$appname" ]; then
          chflags nouchg /Applications/"$appname"/Contents
          rm -rf /Applications/"$appname"
        fi
        cp -Lr "$src" /Applications
        chflags uchg /Applications/"$appname"/Contents
      done

      unset IFS
    ''
    );
  };
}
