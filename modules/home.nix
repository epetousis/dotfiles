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
  ] ++ lib.optionals stdenv.isLinux [
    # Add a nicer potential system font to use (Linux distros ship some real shit fonts)
    inter
    _1password-gui
    _1password
    fira-code-nerdfont
  ];

  fonts.fontconfig.enable = pkgs.stdenv.isLinux;

  # Make this config work better on non-NixOS distros
  targets.genericLinux.enable = pkgs.stdenv.isLinux;

  programs.emacs = {
    enable = true;
    package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.emacs29-pgtk else pkgs.emacs-macport.overrideAttrs (old: {
      # Stolen from https://stackoverflow.com/a/68523368/830946
      name="emacs-macport-icon";
      buildCommand = ''
      set -euo pipefail

      ${
        lib.concatStringsSep "\n"
          (map
            (outputName:
              ''
                echo "Copying output ${outputName}"
                set -x
                cp -rs --no-preserve=mode "${pkgs.emacs-macport.${outputName}}" "''$${outputName}"
                set +x
              ''
            )
            (old.outputs or ["out"])
          )
      }

      cp -vf ${./elrumo2.icns} $out/Applications/Emacs.app/Contents/Resources/Emacs.icns
    '';
    });
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
      epkgs.melpaPackages.telega
      epkgs.persp-mode
    ];
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    pinentryFlavor = "qt";
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
      VISUAL = "code --wait";
      EDITOR = "emacs -nw";
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

    eval "$(direnv hook zsh)"
    '';
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = if pkgs.stdenv.hostPlatform.isDarwin then {
        extraOptions = {
          # yes i know there is definitely a way that this could be pure but such is life
          IdentityAgent = "\"~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock\"";
        };
      } else {
        extraOptions = {
          IdentityAgent = "\"~/.1password/agent.sock\"";
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
    # Keep init.el out of Nix store, as emacs has lots of (useful!) config auto-editing functionality.
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.local/share/dotfiles/configs/emacs/init.el";
    target = ".emacs.d/init.el";
  };

  home.file.gnus = {
    source = ../configs/emacs/gnus.el;
    target = ".gnus.el";
  };

  home.file.".npmrc".text = "prefix = ~/.local/share/npm-packages";

  home.sessionPath = [
    "$HOME/.local/share/npm-packages/bin"
  ];

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
