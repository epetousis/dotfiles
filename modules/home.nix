{ lib, pkgs, config, ... }:

{
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    axel
    bitwarden-cli
    deluge
    discordo
    emacs-lsp-booster
    ffmpeg
    fd
    fira-code
    fira-code-nerdfont
    fzf
    git-filter-repo
    heroku
    jq
    kitty
    mosh
    mpv
    neovim
    nil
    nix-output-monitor
    nodejs
    nodePackages.pyright
    nodePackages.typescript-language-server
    rclone
    ripgrep
    rust-analyzer
    streamlink
    syncthing
    tmux
    wget
    wl-clipboard
    youtube-music
    yt-dlp
  ] ++ lib.optionals stdenv.isDarwin [
    # macOS dependencies, stuff that should be everywhere instead of one system
    anki-bin
    cyberduck
    discord
    firefox-bin
    karabiner-elements
    keka
    monitorcontrol
    openai-whisper-cpp
    pinentry_mac
    libreoffice-bin
    soundsource
    stats
    unnaturalscrollwheels
  ] ++ lib.optionals stdenv.isLinux [
    anki
    fira-code-nerdfont
    libreoffice
    source-sans-pro
    source-han-sans
    thunderbird
    vesktop
  ] ++ lib.optionals stdenv.isx86_64 [
    # No x86 only apps at the moment.
  ];

  fonts.fontconfig.enable = pkgs.stdenv.isLinux;

  # Make this config work better on non-NixOS distros
  targets.genericLinux.enable = pkgs.stdenv.isLinux;

  programs.firefox = with pkgs; {
    enable = stdenv.hostPlatform.isLinux;
    package = if stdenv.hostPlatform.isDarwin
              then firefox-bin
              else firefox;
  };

  programs.gpg = {
    enable = true;
  };

  services.pantalaimon = {
    enable = true;
    settings = {
      local-beeper = {
        Homeserver = "https://matrix.beeper.com";
        ListenAddress = "127.0.0.1";
        ListenPort = 58249;
        UseKeyring = true;
      };
    };
  };

  services.gpg-agent = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  # Run the Gnome Keyring daemon
  services.gnome-keyring = {
    enable = true;
    components = [
      "pkcs11"
      "secrets"
      "ssh"
    ];
  };

  # Set path to gnome-keyring's SSH agent
  home.sessionVariables.SSH_AUTH_SOCK="/run/user/1000/keyring/ssh";

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Evan Petousis";
    userEmail = "evan@petousis.net";
    signing = {
      key = "89F675EAFEFE6CA5";
      signByDefault = true;
    };
    aliases = {
      log-graph = "log --graph --oneline --all";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
      pull = {
        rebase = true;
      };
      merge = {
        ff = "only";
      };
    } // lib.optionals pkgs.stdenv.hostPlatform.isLinux {
      credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
    };
  };

  programs.zsh = {
    enable = true;
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.7.0";
          sha256 = "149zh2rm59blr2q458a5irkfh82y3dwdich60s9670kl3cl5h2m1";
        };
      }
      {
        file = "powerlevel10k.zsh-theme";
        name = "powerlevel10k";
        src = "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k";
      }
      {
        file = "p10k.zsh";
        name = "powerlevel10k-config";
        src = ../configs/p10k;
      }
    ];
    sessionVariables = {
      iCloudDrive = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
      FZF_DEFAULT_COMMAND = "rg --files --follow --no-ignore-vcs --hidden -g '!{**/node_modules/*,**/.git/*}'";
    };
    localVariables = {
      # Kill key timeout so escape is instant
      KEYTIMEOUT = 1;
      PROMPT = "%n@%m:%(4~|...|)%3~ %(!.#.$) ";
    };
    initExtra = ''
    # Discourage instinctively opening default macOS Terminal
    if [[ $TERM_PROGRAM == "Apple_Terminal" ]]; then
      tput setab 3;echo "=== STOP! You are using the built-in macOS terminal when you have opted for an alternative terminal. ==="
      tput setab 3;echo "Disregard if opening Terminal was intended."
    fi

    # If this distro uses GSSAPIKexAlgorithms in its config, fallback to system SSH for Git!
    if cat /etc/crypto-policies/back-ends/openssh.config 2>/dev/null | rg -q GSSAPIKexAlgorithms ; then
      export GIT_SSH=/usr/bin/ssh
    fi

    bindkey "^H" backward-delete-char
    bindkey "^?" backward-delete-char

    # Enable Ctrl-x-e for CLI editing
    autoload -U edit-command-line
    zle -N edit-command-line
    bindkey '^xe' edit-command-line
    bindkey '^x^e' edit-command-line

    alias nxrb='${if pkgs.stdenv.hostPlatform.isDarwin then "darwin-rebuild" else "sudo nixos-rebuild"} switch --flake ~/.local/share/dotfiles'

    eval "$(direnv hook zsh)"

    # Set path in zsh init to work around `home.sessionPath` being broken
    export PATH="$HOME/.local/share/npm-packages/bin":$PATH
    '';
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      "*" = {
        # Potentially unnecessary, as Gnome Keyring should see id_rsa automatically.
        identityFile = "~/.ssh/id_rsa";
      };
    };
  };

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    escapeTime = 0;
    extraConfig = ''
    set -g set-clipboard external

    # Modify bindings that create new shells to ensure they use PWD
    bind '"' split-window -c "#{pane_current_path}"
    bind % split-window -h -c "#{pane_current_path}"
    bind c new-window -c "#{pane_current_path}"
    '';
  };

  home.file.emacs = {
    source = ../configs/emacs/init.el;
    target = ".emacs.d/init.el";
  };

  home.file.".npmrc".text = "prefix = ~/.local/share/npm-packages";

  # Add direnv support - among other things, this can be used for automatically loading shell.nix files
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    # Add layout_poetry from https://github.com/direnv/direnv/issues/592#issuecomment-856227234
    stdlib = ''
      layout_poetry() {
        if [[ ! -f pyproject.toml ]]; then
          log_error 'No pyproject.toml found. Use `poetry new` or `poetry init` to create one first.'
          exit 2
        fi

        local VENV=$(poetry env info --path)
        if [[ -z $VENV || ! -d $VENV/bin ]]; then
          log_error 'No poetry virtual environment found. Use `poetry install` to create one first.'
          exit 2
        fi

        export VIRTUAL_ENV=$VENV
        export POETRY_ACTIVE=1
        PATH_add "$VENV/bin"
      }
    '';
  };

  programs.home-manager.enable = true;
}
