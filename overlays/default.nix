final: prev: {
  discord = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./discord { discord = prev.discord; } else prev.discord;

  spotify = if prev.stdenv.hostPlatform.isLinux then final.callPackage ./spotify { spotify = prev.spotify; } else prev.spotify;

  discord-screenaudio = final.libsForQt5.callPackage ./discord-screenaudio {};

  webcord = final.callPackage ./webcord {};

  webcord-keybinds = final.callPackage ./webcord/keybinds.nix { webcord = final.webcord; };

  mpv = import ./mpv { inherit prev final; };

  lutris = prev.lutris.override {
    extraPkgs = pkgs: [ pkgs.libunwind ];
  };

  gnomeExtensions = prev.gnomeExtensions // {
    scaletoggle = final.callPackage ./scaletoggle {};
  };

  evansEmacs = final.callPackage ./emacs {};

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

  openai-whisper-cpp = prev.openai-whisper-cpp.override { cudaSupport = true; };

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
}
