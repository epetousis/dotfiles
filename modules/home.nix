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

  home.shellAliases = {
    nxrb = "${if pkgs.stdenv.hostPlatform.isDarwin
              then "darwin-rebuild"
              else "nixos-rebuild"} --flake ~/.local/share/dotfiles";
  };

  home.sessionPath = [
    "$HOME/.local/share/npm-packages/bin"
  ];

  home.sessionVariables = {
    iCloudDrive = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
    FZF_DEFAULT_COMMAND = "rg --files --follow --no-ignore-vcs --hidden -g '!{**/node_modules/*,**/.git/*}'";
  };

  programs.fish.enable = true;

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
    pinentry.package = pkgs.pinentry-gnome3;
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
    package = pkgs.gitFull;
    signing = {
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChegsj+4jJRiuVi8Ci7zS/Gm7VvBnXBB4LnHbAEBgv5+O0sB059ejc5Qc4ErqKYIc97/Kyh/twvhKOmitGMpr2O70fTzgZBr0jHW5vJNsH9vT0QuQ49rXxF3TXPaYY0TcsQgUg0oOkJdnLovX+KMDDWZKGzPaRV88A/OiJjTIp+beWieYjK5CtApya1ER04bTsnbRq9WGI6U62ypDjbR4R4BImQvpFFy7fTsQutPOA4P1F8qgK58O1cHhcg4HJ51fZZDoRc2UQnn5wWi4grPBaqVx9Z9gSfVR7FJvgrhpi8q9KWuMoVwrvKw3LOSOq6f/NZ9acRdb1vwcZ+VyJY4ikprfe6LpypklDqsklpXkzNZny1z2zoByKryQa5UuDCyybwtQPX+zAC8DxqH55un04ryAcFLXYDbpgRnI/pov03Vjs71BKNW35eyCCijmv33KA6WNDS3mGHDcqofNPHtb0hildDS8vNJsFKzybQkM3euN0TaltPRkBPruL7QCrYersLedI/py6VvUqeU3LOyiwCs6nHMd5DTAxGH92ElPNbLximnqZfEjMQ0J8C7CNXG8cg3ZjJG9tuz1+NH3jfSxJ8UpjTuntrRMFbKvzz1HBspgFGYCkVP3fdFvojCk0a+MneiUmovDatfQmckb3sLjmSmxyCppu4bTtZRn48tK2JQ==";
      signByDefault = true;
      # Don't reference _1password-gui on Darwin hosts, as nixpkgs has marked it as broken due to the lack of an activation script that links to /Applications. See https://github.com/NixOS/nixpkgs/issues/254944
      signer = if pkgs.stdenv.hostPlatform.isDarwin then "/Applications/1Password.app/Contents/MacOS/op-ssh-sign" else "${pkgs._1password-gui}/bin/op-ssh-sign";
      format = "ssh";
    };
    settings = {
      user = {
        name = "Evan Petousis";
        email = "evan@petousis.net";
      };
      alias = {
        log-graph = "log --graph --oneline --all";
      };
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
          credential.helper = "${pkgs.gitFull}/bin/git-credential-libsecret";
        } else {});
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*" = {
      addKeysToAgent = "yes";
      extraOptions.IdentityAgent = "/run/user/1000/keyring/ssh";
      forwardAgent = false;
      compression = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
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

  # Linking is a bad solution, and I don't install stuff using home-manager anyway.
  targets.darwin.linkApps.enable = false;

  programs.home-manager.enable = true;
}
