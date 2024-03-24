{ lib
, stdenvNoCC
, fetchurl
, unzip
, undmg
}:
let
  buildYear = "2024";
  buildMonth = "01";
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "bluos-controller";
  version = "4.2.1";

  src = fetchurl {
    url = "https://content-bluesound-com.s3.amazonaws.com/uploads/${buildYear}/${buildMonth}/BluOS-Controller-${finalAttrs.version}-MacOS.zip";
    sha256 = "sha256-kJbx8qznHquegZzkxATpAExMo6TXaKTMAeKoYXZM9gY=";
  };
  dontUnpack = true;

  # Yes, they use both formats for some reason.
  nativeBuildInputs = [ unzip undmg ];

  installPhase = ''
    runHook preInstall

    DMG_NAME="BluOS Controller ${finalAttrs.version}.dmg"

    mkdir -p $out/Applications
    unzip $src "$DMG_NAME"
    undmg "$DMG_NAME"
    mv "BluOS Controller.app" $out/Applications/

    runHook postInstall
  '';

  meta = with lib; {
    description = "BluOS device controller";
    homepage = "https://www.bluesound.com";
    license = licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    maintainers = with maintainers; [ epetousis ];
    platforms = platforms.darwin;
  };
})
