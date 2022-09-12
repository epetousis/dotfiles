{ stdenv }:

stdenv.mkDerivation {
  name = "xcbstub";

  src = ./.;

  buildPhase = ''
  gcc xcbstub.c -o xcbstub.so -fPIC -shared
  '';

  installPhase = ''
  mkdir -p $out/lib
  mv xcbstub.so $out/lib
  '';
}
