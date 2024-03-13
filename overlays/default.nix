final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./discord { discord = prev.discord; } else prev.discord;

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

  # Fix build of openai-whisper on macOS
  openai-triton-llvm = prev.openai-triton-llvm.override (p: {
    # OpenAI Triton doesn't seem to need this at all, not sure why it is here?
    libpfm = if prev.stdenv.hostPlatform.isDarwin then null else p.libpfm;
  });
}
