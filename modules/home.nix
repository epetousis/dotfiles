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

  home.packages = with pkgs; [
    axel
    ffmpeg
    fd
    fzf
    git-filter-repo
    heroku
    jq
    mosh
    neovim
    nodejs
    nodePackages.pyright
    nodePackages.typescript-language-server
    rclone
    ripgrep
    rust-analyzer
    streamlink
    thefuck
    tmux
    wget
    yt-dlp
  ] ++ lib.optionals stdenv.isDarwin [
    # macOS dependencies
    pinentry_mac
  ];

  programs.emacs = {
    enable = true;
    package = if pkgs.stdenv.hostPlatform.isLinux then
        if pkgs.stdenv.hostPlatform.isx86 then pkgs.emacsPgtkNativeComp else (pkgs.emacs.override { withSQLite3 = true; withGTK3 = true; })
    else pkgs.emacs;
    extraPackages = epkgs: [
      # emacs packages
      epkgs.company
      epkgs.dtrt-indent
      epkgs.editorconfig
      epkgs.eglot
      epkgs.envrc
      epkgs.flymake-eslint
      epkgs.magit
      epkgs.monokai-theme
      epkgs.use-package

      # Modes
      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.typescript-mode
      epkgs.web-mode
    ];
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
  };

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
    };
  };

  programs.zsh = {
    enable = true;
    sessionVariables = {
      iCloudDrive = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
      FZF_DEFAULT_COMMAND = "rg --files --follow --no-ignore-vcs --hidden -g '!{**/node_modules/*,**/.git/*}'";
    };
    localVariables = {
      VISUAL = "emacsclient -a emacs";
      EDITOR = "emacs";
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

    bindkey "^H" backward-delete-char
    bindkey "^?" backward-delete-char

    eval "$(direnv hook zsh)"
    '';
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = {
        identityFile = "~/.ssh/id_rsa";
        extraOptions = {
          IgnoreUnknown = "AddKeysToAgent,UseKeychain";
          UseKeychain = "yes";
          AddKeysToAgent = "yes";
        };
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

  home.file.gnus = {
    source = ../configs/emacs/gnus.el;
    target = ".gnus.el";
  };

  home.file.p10k = {
    source = ../configs/p10k/p10k.zsh;
    target = ".p10k.zsh";
  };

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

  # Symlink macOS apps - taken from https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
  home.activation = lib.mkIf pkgs.stdenv.isDarwin {
    copyApplications = let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      baseDir="$HOME/Applications/Home Manager Apps"
      if [ -d "$baseDir" ]; then
        rm -rf "$baseDir"
      fi
      mkdir -p "$baseDir"
      for appFile in ${apps}/Applications/*; do
        target="$baseDir/$(basename "$appFile")"
        $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
        $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      done
    '';
  };
}
