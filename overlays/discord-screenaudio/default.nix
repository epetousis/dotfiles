{ stdenv
, fetchFromGitHub
, lib
, cmake
, pkg-config
, wrapQtAppsHook
, qtbase
, qtwebengine
, knotifications
, pipewire
, xdg-desktop-portal-gtk
}:

# FIXME: I haven't had much luck getting this to work, at least under Wayland.
# Two issues occur - the discord-screenaudio-virtmic device can't be found (or at least, it has no label),
# and even if you get past this, screen sharing simply doesn't work.
# Possibly related to https://github.com/NixOS/nixpkgs/issues/91218

stdenv.mkDerivation rec {
  name = "discord-screenaudio";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "maltejur";
    repo = name;
    rev = "v${version}";
    sha256 = "sha256-HyX3sVuFUHIH7ex31x14Jqwt5UIn1aEt5SnTGDxgNxc=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake pkg-config ];
  buildInputs = [
    wrapQtAppsHook
    qtbase
    qtwebengine
    knotifications
    pipewire
    xdg-desktop-portal-gtk
  ];

  # TODO: make a patch instead of whatever this is
  prePatch = ''
sed -i -e "59i target_include_directories($\{PROJECT_NAME} SYSTEM PRIVATE ${pipewire.dev}/include/pipewire-0.3 ${pipewire.dev}/include/spa-0.2)" submodules/rohrkabel/CMakeLists.txt
'';

  qtWrapperArgs = [ "--set QT_QPA_PLATFORM wayland" ];

  meta = with lib; {
    description = "A very WIP custom discord client that supports streaming with audio on Linux.";
    homepage = "https://github.com/maltejur/discord-screenaudio";
    license = licenses.gpl3;
    platforms = platforms.linux;
    broken = true;
  };
}
