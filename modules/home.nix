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

  # Allow unfree packages in Nix flake commands (still requires `--impure`)
  nixpkgs.config.allowUnfree = true;

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

  services.gpg-agent = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  # Run the Gnome Keyring daemon
  services.gnome-keyring = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    components = [
      "pkcs11"
      "secrets"
      "ssh"
    ];
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Evan Petousis";
    userEmail = "evan@petousis.net";
    signing = {
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ==";
      signByDefault = true;
      gpgPath = lib.mkForce "";
    };
    iniContent = {
      gpg.format = "ssh";
      # Don't reference _1password-gui on Darwin hosts, as nixpkgs has marked it as broken due to the lack of an activation script that links to /Applications. See https://github.com/NixOS/nixpkgs/issues/254944
      "gpg \"ssh\"".program = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) "${pkgs._1password-gui}/bin/op-ssh-sign";
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
    } // (if pkgs.stdenv.hostPlatform.isLinux
        then {
          credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
        } else {});
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

    alias nxrb='${if pkgs.stdenv.hostPlatform.isDarwin
                  then "darwin-rebuild"
                  else "sudo nixos-rebuild"} --flake ~/.local/share/dotfiles --log-format multiline-with-logs'

    alias e='emacsclient -r --no-wait'

    eval "$(direnv hook zsh)"

    # Set path in zsh init to work around `home.sessionPath` being broken
    export PATH="$HOME/.local/share/npm-packages/bin":$PATH
    '';
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks."*".extraOptions.IdentityAgent = "/run/user/1000/keyring/ssh";
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
