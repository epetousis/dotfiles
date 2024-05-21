{ lib, stdenv, fetchFromGitHub, substituteAll, python310, python310Packages }:

let
  python = python310.withPackages(pkgs: [pkgs.dbus-python]);
in
stdenv.mkDerivation rec {
  pname = "gnome-shell-extension-scaletoggle";
  version = "unstable-2024-05-21";

  src = fetchFromGitHub {
    owner = "epetousis";
    repo = "ScaleToggle";
    rev = "bed1683b82616eeb4a8f83ab0355e8f1f504bdc0";
    sha256 = "sha256-Qp04Xya2x89GAM/NXAAz90S9BVU5f2YELG6jUDRsNj4=";
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
    license = licenses.free;
    # maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}
