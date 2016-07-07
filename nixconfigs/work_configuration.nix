# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let
  literals = import ./literals.nix {pkgs = pkgs;};
in

{
  require = [
      ./work_hardware-configuration.nix
      ./private.nix
  ];

  boot = {
    kernelPackages      = pkgs.linuxPackages_latest;
    loader.grub = {
      timeout = 1;
      enable  = true;
      version = 2;
      device  = "/dev/sda";
    };
  };

  nix = {
    package             = pkgs.nixUnstable;
    binaryCaches        = [ http://cache.nixos.org ];
    trustedBinaryCaches = [ http://cache.nixos.org ];
    useChroot           = true;
    gc = {
      automatic = true;
      dates     = "2 weeks";
    };
  };
  nixpkgs.config = {
    allowUnfree = true;
  };

  networking = {
    firewall = {
      enable = false;
      allowedUDPPorts = [ 7777 ];
      allowedTCPPorts = [ 7777 ];
    };
    hostName       = "aseroka-nixos";
    extraHosts     = literals.workHosts;
    proxy.default  = "rain.ifmo.ru:3128";
    connman.enable = true;
  };

  i18n = {
    consoleFont   = "lat9w-16";
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
    opengl = {
      driSupport32Bit = true;
    };
    pulseaudio.enable      = true;
  };

  security.sudo.configFile = literals.sudoConf;

  programs.ssh.startAgent = false;

  services = {
    dbus.enable            = true;
    nixosManual.showManual = true;
    locate.enable          = true;
    udisks2.enable         = true;
    openssh.enable         = true;
    printing.enable        = true;
    ntp.enable             = true;
  };

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us,ru(winkeys)";
    xkbOptions = "grp:caps_toggle";
    xkbVariant = "winkeys";
    displayManager.slim = {
      enable = true;
      autoLogin = false;
      defaultUser = "jaga";
      theme = pkgs.fetchurl {
          url    = https://github.com/jagajaga/nixos-slim-theme/archive/1.1.tar.gz;
          sha256 = "66c3020a6716130a20c3898567339b990fbd7888a3b7bbcb688f6544d1c05c31";
        };
    };
    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable                 = true;
        enableContribAndExtras = true;
      };
    };
    startGnuPGAgent = true;
  };

  virtualisation.libvirtd.enable = true;

  users.extraUsers.jaga = {
    description = "Arseniy Seroka";
    createHome  = true;
    home        = "/home/jaga";
    group       = "users";
    extraGroups = [ "wheel" "networkmanager" "adb" "video" "power" "vboxusers" "libvirtd" ];
    shell       = "${pkgs.zsh}/bin/zsh";
    uid         = 1000;
  };

  time.timeZone = "Europe/Moscow";

  environment.systemPackages = with pkgs; [
   bash
   connmanui
   git
   htop
   iotop
   mc
   pmutils
   stdenv
   wget
   xsel
   zsh
   ceph
   /*cjdns*/
  ];
  fonts = {
    enableFontDir          = true;
    enableGhostscriptFonts = true;
    fonts = [
    ];
  };
}
