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
    pop-shell = prev.gnomeExtensions.pop-shell.overrideAttrs {
      version = "unstable-2024-04-04";
      src = final.fetchFromGitHub {
        owner = "pop-os";
        repo = "shell";
        rev = "cfa0c55e84b7ce339e5ce83832f76fee17e99d51";
        sha256 = "sha256-IQJtTMYCkKyjqDKoR35qsgQkvXIrGLq+qtMDOTkvy08=";
      };
    };
  };

  emacsPackages = prev.emacsPackages // {
    nano-theme-git = final.emacsPackages.callPackage ./emacs-nano-theme {};
    evansEmacs = final.callPackage ./emacs {};
  };

  weechat = prev.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = with prev.weechatScripts; [
        wee-slack
      ];
      plugins = builtins.attrValues (builtins.removeAttrs availablePlugins [ "php" ]);
    };
  };

  bluos-controller = final.callPackage ./bluos-controller {};

  asahi-btsync = final.callPackage ./asahi-btsync.nix {};

  openai-whisper-cpp = import ./openai-whisper-cpp { inherit prev final; };

  rclone = prev.callPackage "${prev.path}/pkgs/applications/networking/sync/rclone" {
    buildGoModule = args: prev.buildGoModule (args // {
      version = "unstable-2024-04-11";
      # Add rclone fork with iCloud Drive support (I am very impatient)
      src = final.fetchFromGitHub {
        owner = "lostb1t";
        repo = "rclone";
        rev = "791bc98f440788ba18fe32cde7cbc417874f7330";
        sha256 = "sha256-Nb/gd64MSnMvEA25JRftjwmSlNevhgrHBqUHdRE3Pyk=";
      };
      vendorHash = "sha256-5h47Kh7DKX41mAGdMN9kH88ekjLy2POHzMK+0XcUpz8=";
    });
  };

  apple-color-emoji = final.stdenv.mkDerivation rec {
    name = "apple-color-emoji";
    version = "17.4";
    src = builtins.fetchurl {
      url = "https://github.com/samuelngs/apple-emoji-linux/releases/download/v${version}/AppleColorEmoji.ttf";
      sha256 = "sha256:1wahjmbfm1xgm58madvl21451a04gxham5vz67gqz1cvpi0cjva8";
    };
    dontUnpack = true;
    installPhase = ''
      runHook preInstall

      mkdir -p $out/share/fonts/truetype
      cp $src $out/share/fonts/truetype/AppleColorEmoji.ttf

      runHook postInstall
    '';
  };

  vesktop = prev.vesktop.overrideAttrs (p: {
    version = "unstable-2024-04-21";
    src = final.fetchFromGitHub {
      owner = "Vencord";
      repo = "Vesktop";
      rev = "2733727a40a4cf542277dedcf89e87e7740f962d";
      hash = "sha256-EF36HbbhTuAdwBEKqYgBBu7JoP1LJneU78bROHoKqDw=";
    };

    pnpmDeps = p.pnpmDeps.overrideAttrs {
      outputHash = "sha256-6ezEBeYmK5va3gCh00YnJzZ77V/Ql7A3l/+csohkz68=";
    };
  });

  kdePackages = prev.kdePackages.overrideScope (f: p: {
    kwin = p.kwin.overrideAttrs (kprev: rec {
      version = "463ae633e878004b1799f618641a0c44573c10f4";
      src = prev.fetchFromGitLab {
        owner = "plasma";
        repo = "kwin";
        rev = version;
        sha256 = "sha256-IMwhAaeakM8hMiS5K1ZijaqV9Y4DXbq5Gfnutv4JwBg=";
        domain = "invent.kde.org";
      };
    });
  });
}
