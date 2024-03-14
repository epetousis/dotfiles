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

  pidginPackages = prev.pidginPackages.overrideScope (pfinal: pprev: {
    pidgin = pprev.pidgin.overrideAttrs (p: {
      # FIXME: libxml on nixpkgs-unstable changes some function signatures, ignore those errors to get the build to work
      env.NIX_CFLAGS_COMPILE = p.env.NIX_CFLAGS_COMPILE + " -Wno-error=incompatible-function-pointer-types -Wno-error=int-conversion";
    });

    purple-discord = pprev.purple-discord.overrideAttrs (p: {
      version = "unstable-2023-12-28";
      src = prev.fetchFromGitHub {
        owner = "EionRobb";
        repo = "purple-discord";
        rev = "b9253821e106f070def20e5cf9b4ad6aa4a812ac";
        sha256 = "sha256-f9nDrK/5Pd+4to/VjpS6zKzKk1bp4g9QmnFHJTzNgSE";
      };
      nativeBuildInputs = p.nativeBuildInputs ++ [ prev.gcc prev.pkg-config ];
      buildInputs = p.buildInputs ++ [ prev.qrencode prev.nss ];
      buildPhase = ''
        # -pipe breaks the build...
        substituteInPlace Makefile --replace-warn '-O2 -g -pipe -Wall' '-O2 -g -Wall'
        make
      '';
      meta = p.meta // {
        platforms = p.meta.platforms ++ prev.lib.platforms.darwin;
      };
    });
  });
}
