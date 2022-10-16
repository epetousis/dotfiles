# Based on fufexan's work, originally licensed under the MIT license: https://github.com/fufexan/webcord-flake
/*
MIT License

Copyright (c) 2022 Mihai Fufezan

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
{
  lib,
  pkgs,
  ...
}:

let
  desktopItem = pkgs.makeDesktopItem {
    name = "webcord";
    desktopName = "WebCord";
    genericName = "Discord and Fosscord client";
    exec = "webcord";
    icon = "webcord";
    categories = ["Network" "InstantMessaging"];
    mimeTypes = ["x-scheme-handler/discord"];
  };
in
pkgs.mkYarnPackage rec {
  name = "webcord";
  version = "3.8.6";

  src = pkgs.fetchFromGitHub {
    owner = "SpacingBat3";
    repo = name;
    rev = "v${version}";
    sha256 = "sha256-1lqW55dRmkFrNzXR0+Lb1gxz3WZEzw7iB36MFRpKtUk=";
  };

  yarnLock = ./yarn.lock;

  nativeBuildInputs = with pkgs; [
    autoPatchelfHook
    wrapGAppsHook
    jq
    makeWrapper
    nodejs
    python3
    stdenv.cc.cc
  ];

  buildInputs = with pkgs;
    [
      alsa-lib
      atk
      at-spi2-atk
      at-spi2-core
      cairo
      cups
      dbus
      expat
      fontconfig
      freetype
      gdk-pixbuf
      glib
      gtk3
      libappindicator-gtk3
      libcxx
      libdbusmenu
      libdrm
      libnotify
      libpulseaudio
      libuuid
      mesa
      nspr
      nss
      pango
      stdenv.cc.cc
      systemd
      vulkan-loader
    ]
    ++ (with pkgs.xorg; [
      libX11
      libxcb
      libXcomposite
      libXcursor
      libXdamage
      libXext
      libXfixes
      libXi
      libXrandr
      libXrender
      libXScrnSaver
      libXtst
    ]);

  buildPhase = ''
    runHook preBuild
    yarn build
    runHook postBuild
  '';

  postBuild = ''
    # Copy translations which are not included in build
    cp -r deps/webcord/sources/translations deps/webcord/app
    # Remove husky git hook (useless and breaks shebang patch)
    rm -rf deps/webcord/.husky
  '';

  postInstall = ''
    mkdir -p $out/share/icons/hicolor
    for res in {24,48,64,128,256}; do
      mkdir -p $out/share/icons/hicolor/''${res}x''${res}/apps
      ln -s $out/libexec/webcord/deps/webcord/sources/assets/icons/app.png \
        $out/share/icons/hicolor/''${res}x''${res}/apps/webcord.png
    done
    ln -s "${desktopItem}/share/applications" $out/share/
  '';

  # Use Electron 20 as there appears to be a bug in 21 that causes crashing on longer voice calls
  postFixup = ''
    makeWrapper ${pkgs.electron_20}/bin/electron $out/bin/webcord \
      --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform=wayland}}" \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [pkgs.pipewire]}" \
      --prefix PATH : "${lib.makeBinPath [pkgs.xdg-utils]}" \
      --add-flags $out/libexec/webcord/deps/webcord
  '';
}
