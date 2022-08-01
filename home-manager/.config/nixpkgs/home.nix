{ config, pkgs, ... }:

{
  home.username = "epetousis";
  home.homeDirectory = "/Users/epetousis";

  home.packages = with pkgs; [
    axel
    ffmpeg
    fd
    fzf
    git
    git-filter-repo
    gnupg
    heroku
    jq
    neovim
    openvscode-server
    pinentry
    pinentry_mac
    rclone
    ripgrep
    streamlink
    thefuck
    tmux
    wget
    yt-dlp
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

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
}
