{ wrapMpv, mpv-unwrapped, mpv, stdenv }:

if stdenv.hostPlatform.isLinux
# Rewrap mpv with special args on Linux only
then wrapMpv mpv-unwrapped {
  extraMakeWrapperArgs = [
    # Forces use of GLES as GBM support isn't fully supported in Gnome yet.
    # See: https://github.com/mpv-player/mpv/wiki/FAQ#nvidia-and-wayland
    "--add-flags" "--opengl-es=yes"
    # Disable vsync to fix dropped frames when mpv is in background on Nvidia
    "--add-flags" "--wayland-disable-vsync"
  ];
}
else mpv
