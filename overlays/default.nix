final: prev:
let
  overlays = [
    (import ./package.nix)
    (import ./emacs/overlay.nix)
  ];
in
prev.lib.composeManyExtensions overlays final prev
