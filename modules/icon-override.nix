{ pkgs, pkg, iconPath, lib }:
/*
Often, one will come across a macOS app where a developer either has forgotten to update their
icon styling to match Apple's redesigns, or worse: [a developer has refused outright to implement it.](https://github.com/alacritty/alacritty/pull/7110#pullrequestreview-1542387778).
Maybe one just wants their icons to match a certain style. This derivation helps with this.
*/
pkg.overrideAttrs (old: {
  name="${old.pname}-macport-icon";

  # Stolen from https://stackoverflow.com/a/68523368/830946
  buildCommand = ''
      set -euo pipefail
      PATH=${lib.makeBinPath [ pkgs.xcbuild pkgs.jq ]}:$PATH

      ${
        lib.concatStringsSep "\n"
          (map
            (outputName:
              ''
                echo "Copying output ${outputName}"
                set -x
                cp -rs --no-preserve=mode "${pkg.${outputName}}" "''$${outputName}"
                set +x
              ''
            )
            (old.outputs or ["out"])
          )
      }

      APP_PATH=$(for i in $out/Applications/*.app; do printf '%s\n' "$i"; break; done)

      plutil -lint $APP_PATH/Contents/Info.plist
      ICON=$(plutil -convert json -o - $APP_PATH/Contents/Info.plist | jq -r .CFBundleIconFile)

      # TODO: find .app smarter
      cp -vf ${iconPath} $APP_PATH/Contents/Resources/$ICON
    '';
})
