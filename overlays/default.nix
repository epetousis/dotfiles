final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then prev.callPackage ./discord { discord = prev.discord; } else prev.discord;

  yabai = prev.callPackage ./yabai { yabai = prev.yabai; };

  spotify = if prev.stdenv.hostPlatform.isLinux then prev.callPackage ./spotify { spotify = prev.spotify; } else prev.spotify;

  discord-screenaudio = prev.callPackage ./discord-screenaudio {};

  webcord = prev.callPackage ./webcord {};

  webcord-keybinds = prev.callPackage ./webcord/keybinds.nix { webcord = final.webcord; };

  mpv = prev.callPackage ./mpv { mpv = prev.mpv; };
}
