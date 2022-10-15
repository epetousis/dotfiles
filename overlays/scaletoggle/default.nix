{ lib, stdenv, fetchFromGitHub, substituteAll, python310, python310Packages }:

let
  python = python310.withPackages(pkgs: [pkgs.dbus-python]);
in
stdenv.mkDerivation rec {
  pname = "gnome-shell-extension-scaletoggle";
  version = "unstable-2022-10-15";

  src = fetchFromGitHub {
    owner = "epetousis";
    repo = "ScaleToggle";
    rev = "82f6a366919911e33274b39ad0ebde3063fdf3a1";
    sha256 = "sha256-oNJkeZaa6yYQwhpmUinNJXG6YD/k0vvAOQ4UAfuixSc=";
  };

  nativeBuildInputs = [ python ];

  postPatch = ''
    substituteInPlace extension.js \
      --replace python3 ${python}/bin/python3 \
      --replace .local/share $out/share
    substituteInPlace scaletoggle.py \
      --replace /usr/bin/python3 ${python}/bin/python3
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/scaletoggle@t-vk.github.com
    cp -r * $out/share/gnome-shell/extensions/scaletoggle@t-vk.github.com
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "scaletoggle@t-vk.github.com";
  };

  meta = with lib; {
    description = "Adds a toggle to change the display scaling of the primary monitor between 100% and 200%";
    homepage = "https://github.com/epetousis/ScaleToggle";
    license = null;
    # maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}
