final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then prev.callPackage ./discord { discord = prev.discord; } else prev.discord;

  yabai = prev.callPackage ./yabai { yabai = prev.yabai; };

  spotify = if prev.stdenv.hostPlatform.isLinux then prev.callPackage ./spotify { spotify = prev.spotify; } else prev.spotify;
}
