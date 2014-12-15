# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let
  literals = import ./literals.nix {pkgs = pkgs;
                ca   = pkgs.writeText "ca.crt" (builtins.readFile /root/.vpn/ca.crt);
                cert = pkgs.writeText "alice" (builtins.readFile /root/.vpn/alice.crt);
                key  = pkgs.writeText "alice.key" (builtins.readFile /root/.vpn/alice.key);  };
in

{
  require = [
      ./desktop_hardware-configuration.nix
      ./private.nix
  ];

  boot = {
    kernelPackages      = pkgs.linuxPackages_3_17;
    extraModprobeConfig = ''
      options snd slots=snd_usb_audio,snd-hda-intel
    '';
    loader.grub = {
      timeout = 1;
      enable  = true;
      version = 2;
      device  = "/dev/sdc";
    };
  };

  nix = {
    package             = pkgs.nixUnstable;
    binaryCaches        = [ http://cache.nixos.org ];
    trustedBinaryCaches = [ http://cache.nixos.org ];
    useChroot           = builtins.trace (if config.networking.hostName == "nixos" then "1" else "2") true;
    gc = {
      automatic = true;
      dates     = "2 weeks";
    };
  };
  nixpkgs.config = {
    allowUnfree             = true;
    virtualbox.enableExtensionPack = true;
  };


  networking = {
    firewall = {
      allowedUDPPorts = [ 7777 ];
      allowedTCPPorts = [ 7777 ];
    };
    hostName                 = "nixos";
    extraHosts               = literals.extraHosts;
    connman.enable           = true;
  };

  fileSystems."/home" = {
    device          = "/dev/sdc2";
    fsType          = "ext4";
    options         = "data=journal,users,rw,user,auto,exec";
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
  };

  security.sudo.configFile = literals.sudoConf;

  services = {
    dbus.enable            = true;
    nixosManual.showManual = true;
    locate.enable          = true;
    udisks2.enable         = true;
    openssh.enable         = true;
    printing.enable        = true;
    ntp.enable             = true;
    mysql = {
      enable  = true;
      package = pkgs.mysql;
    };
    openvpn = {
      enable         = true;
      servers.client = literals.openVPNConf;
    };
    virtualboxHost.enable = true;
  };
  /*services.tor.client.enable = true;*/
  /*services.cjdns.enable = true;*/

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    videoDrivers = [ "nvidia" ];
    layout = "us,ru(winkeys)";
    xkbOptions = "grp:caps_toggle";
    xkbVariant = "winkeys";
    displayManager.slim = {
      enable = true;
      autoLogin = true;
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
    config = literals.trackBallConf;
  };

  sound.extraConfig = literals.alsaConf;

  users.extraUsers.jaga = {
    description = "Arseniy Seroka";
    createHome  = true;
    home        = "/home/jaga";
    group       = "users";
    extraGroups = [ "wheel" "networkmanager" "adb" "video" "power" "vboxusers" ];
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
   /*cjdns*/
  ];
  fonts = {
    /*fontconfig.enable = true;*/
    enableFontDir          = true;
    enableGhostscriptFonts = true;
    /*fontconfig.defaultFonts.monospace = ["terminus"];*/
    fonts = [
       pkgs.corefonts
       pkgs.clearlyU
       pkgs.cm_unicode
       pkgs.dejavu_fonts
       pkgs.freefont_ttf
       pkgs.terminus_font
       pkgs.ttf_bitstream_vera
    ];
  };
}
