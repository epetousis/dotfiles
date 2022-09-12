{ symlinkJoin, discord, makeWrapper, ... }:

symlinkJoin {
    name = discord.name;
    paths = [ discord.out ];
    buildInputs = [ makeWrapper ];
    postBuild = "wrapProgram $out/bin/Discord --add-flags '--use-gl=desktop'";
}
