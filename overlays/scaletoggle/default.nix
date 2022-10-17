{ lib, stdenv, fetchFromGitHub, substituteAll, python310, python310Packages }:

let
  python = python310.withPackages(pkgs: [pkgs.dbus-python]);
in
stdenv.mkDerivation rec {
  pname = "gnome-shell-extension-scaletoggle";
  version = "unstable-2022-10-17";

  src = fetchFromGitHub {
    owner = "epetousis";
    repo = "ScaleToggle";
    rev = "47f429137cc4d0e7892f85d9e9fe2457def9fa02";
    sha256 = "sha256-Y2MfuVwjb3YGLWbn7JdwNt5ivmebamqN3vFqKxrYeHI=";
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
