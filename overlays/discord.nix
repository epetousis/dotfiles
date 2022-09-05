{ pkgs, ... }:

final: prev: {
  discord = pkgs.symlinkJoin {
    name = prev.discord.name;
    paths = [ prev.discord ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = "wrapProgram $out/bin/Discord --add-flags '--use-gl=desktop'";
  };
}
