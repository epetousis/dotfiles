{ symlinkJoin
, spotify
, makeWrapper
, callPackage
}:

/*
Patches Spotify to force-enable Wayland support.
Wayland support is buggy - the titlebar is not drawn by Spotify's Electron, so Gnome will simply display no titlebar.
Use Super + Mouse1 to move the window, and Super + Mouse3 to resize.
Spotify will also not quit after closing the window - you will need to use killall.
*/
symlinkJoin {
  name = spotify.name;
  paths = [ spotify.out ];
  buildInputs = [ makeWrapper ];
  postBuild = ''wrapProgram $out/bin/spotify \
  --set LD_PRELOAD ${callPackage (import ./xcbstub) {}}/lib/xcbstub.so \
  --add-flags "--enable-features=UseOzonePlatform --ozone-platform=wayland"'';
}
