 /*
 This cannot be built from source as it requires entitlements and
 for that it needs to be code signed.
 */
{ yabai, ... }:

yabai.overrideAttrs(
  o: rec {
  pname = "yabai";
  version = "4.0.2";

  src = fetchTarball {
      url = "https://github.com/koekeishiya/${pname}/releases/download/v${version}/${pname}-v${version}.tar.gz";
      sha256 = "00nxzk1g0hd8jqd1r0aig6wdsbpk60551qxnvvqb9475i8qbzjf6";
  };

  installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/share/man/man1/
      cp ./bin/yabai $out/bin/yabai
      cp ./doc/yabai.1 $out/share/man/man1/yabai.1
  '';
  }
)
