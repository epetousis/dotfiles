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
    git
    git-filter-repo
    heroku
    jq
    neovim
    nodePackages.pyright
    openvscode-server
    rclone
    ripgrep
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
    # FIXME: emacsPgtkNativeComp awaiting a fix https://github.com/nix-community/emacs-overlay/issues/244
    package = pkgs.emacs;
    extraPackages = epkgs: [
      # emacs packages
      epkgs.use-package
      epkgs.fzf
      epkgs.lsp-mode
      epkgs.lsp-pyright
      epkgs.lsp-ui
      epkgs.company
      epkgs.evil
      epkgs.evil-collection
      epkgs.which-key
      epkgs.typescript-mode
      epkgs.dtrt-indent
      epkgs.envrc
      epkgs.auto-dark
      epkgs.nix-mode
      epkgs.projectile
      epkgs.flymake-diagnostic-at-point
      epkgs.web-mode
      epkgs.flycheck
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
    defaultKeymap = "viins";
    sessionVariables = {
      iCloudDrive = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
      FZF_DEFAULT_COMMAND = "rg --files --follow --no-ignore-vcs --hidden -g '!{**/node_modules/*,**/.git/*}'";
    };
    localVariables = {
      VISUAL = "nvim";
      EDITOR = "nvim";
      # Kill key timeout so escape is instant
      KEYTIMEOUT = 1;
      PROMPT = "%n@%m:%(4~|...|)%3~ %% ";
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

  home.file.ideavimrc = {
    target = ".ideavimrc";
    text = "
    source ~/.vimrc
    map gh <Action>(ShowErrorDescription)
    map gd <Action>(GotoDeclaration)
    ";
  };

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    prefix = "`";
    keyMode = "vi";
    escapeTime = 0;
    extraConfig = ''
    set -g set-clipboard external

    # Copy vim-style
    bind P paste-buffer
    bind-key -T copy-mode-vi v send-keys -X begin-selection
    bind-key -T copy-mode-vi y send-keys -X copy-selection
    bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

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

  home.file.p10k = {
    source = ../configs/p10k/p10k.zsh;
    target = ".p10k.zsh";
  };

  home.file.nvim = {
    source = ../configs/nvim/init.lua;
    target = ".config/nvim/init.lua";
  };

  # Add direnv support - among other things, this can be used for automatically loading shell.nix files
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

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
