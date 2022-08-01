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
}
