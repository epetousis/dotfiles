final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./discord { discord = prev.discord; } else prev.discord;

  yabai = final.callPackage ./yabai { yabai = prev.yabai; };

  spotify = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./spotify { spotify = prev.spotify; } else prev.spotify;

  discord-screenaudio = final.libsForQt5.callPackage ./discord-screenaudio {};

  webcord = final.callPackage ./webcord {};

  webcord-keybinds = final.callPackage ./webcord/keybinds.nix { webcord = final.webcord; };

  mpv = final.callPackage ./mpv { mpv = prev.mpv; };

  lutris = prev.lutris.override {
    extraPkgs = pkgs: [ pkgs.libunwind ];
  };

  gnomeExtensions = prev.gnomeExtensions // {
    scaletoggle = final.callPackage ./scaletoggle {};
    pop-shell = final.callPackage ./pop-shell {};
  };

  emacsPackages = prev.emacsPackages // {
    nano-theme-git = final.emacsPackages.callPackage ./emacs-nano-theme {};
  };
}
