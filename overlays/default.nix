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
    pop-shell = prev.gnomeExtensions.pop-shell.overrideAttrs (p: {
      # https://github.com/NixOS/nixpkgs/issues/314969#issuecomment-2136412109
      postInstall = p.postInstall or "" + ''
        # Workaround for NixOS/nixpkgs#92265
        mkdir --parents "$out/share/gsettings-schemas/$name/glib-2.0"
        ln --symbolic "$out/share/gnome-shell/extensions/pop-shell@system76.com/schemas" "$out/share/gsettings-schemas/$name/glib-2.0/schemas"

        # Workaround for NixOS/nixpkgs#314969
        mkdir --parents "$out/share/gnome-control-center"
        ln --symbolic "$src/keybindings" "$out/share/gnome-control-center/keybindings"
      '';
    });
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

  openai-whisper-cpp = prev.openai-whisper-cpp.override { cudaSupport = true; };

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
}
