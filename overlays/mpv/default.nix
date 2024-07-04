{ prev, final }:

if final.stdenv.hostPlatform.isLinux
# Rewrap mpv with special args on Linux only
then prev.mpv.override (p: {
  extraMakeWrapperArgs = [
    # Forces use of GLES as GBM support isn't fully supported in Gnome yet.
    # See: https://github.com/mpv-player/mpv/wiki/FAQ#nvidia-and-wayland
    "--add-flags" "--opengl-es=yes"
    # Disable vsync to fix dropped frames when mpv is in background on Nvidia
    "--add-flags" "--wayland-disable-vsync"
  ];
})
else prev.mpv
