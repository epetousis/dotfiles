# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../modules/gnome-shell-suspend-fix.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "evan-pc"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.utf8";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the (unfortunately proprietary) Nvidia driver.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
  # Enables systemd-based suspend to avoid graphical corruption on wake
  # Temporary bug related to https://forums.developer.nvidia.com/t/corrupted-graphics-upon-resume-gnome-41-x-org-495-44-driver/194565/17
  hardware.nvidia.powerManagement.enable = true;
  services.gnome-shell-suspend-fix.enable = true;

  # Enable Wayland on Nvidia.
  hardware.nvidia.modesetting.enable = true;

  # Enable Xwayland for X compatibility.
  programs.xwayland.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Enable fractional scaling on Gnome Wayland.
  services.xserver.desktopManager.gnome.extraGSettingsOverridePackages = [ pkgs.gnome.mutter ];
  services.xserver.desktopManager.gnome.extraGSettingsOverrides = ''
     [org.gnome.mutter]
     experimental-features=['scale-monitor-framebuffer']
   '';

  # Configure keymap in X11
  services.xserver = {
    layout = "au";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.epetousis = {
    isNormalUser = true;
    description = "Evangelos Petousis";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      firefox-wayland
      git
      spotify
      discord
      easyeffects
      lutris
    ];
    shell = pkgs.zsh;
  };

  # Enable Steam
  # NB: it is *essential* that you restart Steam after switching generations, otherwise Proton will fail to work.
  programs.steam.enable = true;

  # Enable flatpak support
  services.flatpak.enable = true;

  # Enable zsh
  programs.zsh.enable = true;
  environment.shells = with pkgs; [ zsh ];

  home-manager.users.epetousis = import ../../modules/home.nix;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ../../overlays/discord.nix { inherit pkgs; })
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    gnomeExtensions.gsconnect
    gnomeExtensions.appindicator
  ];

  # Enable NTFS support
  boot.supportedFilesystems = [ "ntfs" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable Tailscale service
  services.tailscale.enable = true;
  networking.firewall.allowedUDPPorts = [ 41641 ];
  networking.firewall.checkReversePath = "loose";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
