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
        rev = "8dc8147f8ad6eddf416a61475957a2f7f7528846";
        sha256 = "sha256-kM3YZBPgg2IdSd8vIP6uXzI/6039l3UhIAgOhegIUZU=";
      };
      vendorHash = "sha256-5h47Kh7DKX41mAGdMN9kH88ekjLy2POHzMK+0XcUpz8=";
    });
  };
}
