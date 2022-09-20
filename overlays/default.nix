final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./discord { discord = prev.discord; } else prev.discord;

  yabai = final.callPackage ./yabai { yabai = prev.yabai; };

  spotify = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./spotify { spotify = prev.spotify; } else prev.spotify;

  discord-screenaudio = final.callPackage ./discord-screenaudio {};

  webcord = final.callPackage ./webcord {};

  webcord-keybinds = final.callPackage ./webcord/keybinds.nix { webcord = final.webcord; };

  mpv = final.callPackage ./mpv { mpv = prev.mpv; };
}
