{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use GRUB2 to boot as BIOS (for compatability)
  boot.loader.grub.device = "/dev/disk/by-id/usb-WD_My_Passport_25E1_575834314132385258393746-0:0";

  networking.hostName = "bugbox"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Denver";

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    wget neovim firefox emacs rustup sbcl stack
    atom libreoffice-fresh elixir home-manager
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.tll = {
    isNormalUser = true;
    uid = 1000;
  };

  users.extraUsers.tll.extraGroups = [ "wheel" ];

  # Determines the NixOS release with which your system is to be compatible.
  system.nixos.stateVersion = "unstable";

}