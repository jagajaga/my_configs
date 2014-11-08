# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let 

  priv = import (./private.nix);

in

{
  imports = [ <nixos/modules/programs/virtualbox.nix> ];
  require =
    [ 
      ./desktop_hardware-configuration.nix
      ./private.nix
    ];


  boot = { 
    kernelPackages = pkgs.linuxPackages_3_17;
    loader.grub.timeout = 1;
    extraModprobeConfig = ''
      options snd slots=snd_usb_audio,snd-hda-intel
    '';
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sdc";
  };

  nix = {
    package = pkgs.nixUnstable;
    binaryCaches = [ http://cache.nixos.org ];
    trustedBinaryCaches = [ http://cache.nixos.org ];
    gc = {
      automatic = true;
      dates = "2 weeks";
    };
  };
  nixpkgs.config.allowUnfree = true;

  networking = {
    firewall.allowedUDPPorts = [ 7777 ];
    hostName = "nixos"; 
    connman.enable = true;
    extraHosts = ''
        fc5d:baa5:61fc:6ffd:9554:67f0:e290:7535 nodeinfo.hype
        fcbf:7bbc:32e4:716:bd00:e936:c927:fc14 socialno.de
        fcd5:76e1:c1c2:e946:b266:8543:c1d5:67ac hypeoverflow.com
    '';
  };

  fileSystems."/home" =     # where you want to mount the device
    { device = "/dev/sdc2";  # the device
      fsType = "ext4";      # the type of the partition
      options = "data=journal,users,rw,user,auto,exec";
    };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  security.sudo.configFile = ''
    Cmnd_Alias SUSPEND = /var/run/current-system/sw/sbin/pm-suspend, /var/run/current-system/sw/bin/systemctl suspend

    %users      ALL=NOPASSWD: SUSPEND
  '';

  services.dbus.enable = true;
  services.nixosManual.showManual = true;
  services.locate.enable = true;
  services.udisks2.enable = true;
  services.openssh.enable = true;
  services.printing.enable = true;
  /*services.tor.client.enable = true;*/
  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql;
  services.ntp.enable = true;


  services.openvpn = {
    enable = true;
    servers = 
    {
        client = 
            let 
                ca = pkgs.writeText "ca.crt" (builtins.readFile /root/.vpn/ca.crt);
                cert = pkgs.writeText "alice" (builtins.readFile /root/.vpn/alice.crt);
                key = pkgs.writeText "alice.key" (builtins.readFile /root/.vpn/alice.key);
            in
            {
                config = ''
                    client
                    nobind
                    dev tun
                    redirect-gateway def1
                    ca ${ca}
                    cert ${cert}
                    key ${key}
                    <dh>
                    -----BEGIN DH PARAMETERS-----
                    MEYCQQCQn3sGfqQWQtMH4GNWHeZG6210sE3cssDGitRfv9T9knp00zIquPI3tuRa
                    xywN8CG+Ww/V8kBIgLBfqRqnThdzAgEC
                    -----END DH PARAMETERS-----
                    </dh>

                    <connection>
                    remote 178.62.202.50 1194 udp
                    </connection>

                    <connection>
                    remote 178.62.202.50 443 tcp-client
                    </connection>
                '';
            up = "echo nameserver $nameserver | ''${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
            down = "''${pkgs.openresolv}/sbin/resolvconf -d $dev";
          };
    };
  };

  /*services.cjdns.enable = true;*/

  sound.extraConfig = ''
    defaults.pcm.!card 3
  '';
  


  users.extraUsers.jaga = {
    description = "";
    createHome = true;
    home = "/home/jaga";
    group = "users";
    extraGroups = [ "wheel" "networkmanager" "adb" "video" "power" ];
    shell = "${pkgs.zsh}/bin/zsh";
    uid = 1000;
  };

  hardware.opengl = {
    driSupport32Bit = true;
  };

  hardware.bluetooth.enable = true;

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    videoDrivers = [ "nvidia" ];
    layout = "us,ru(winkeys)";
    xkbOptions = "grp:caps_toggle";
    xkbVariant = "winkeys";
    displayManager.slim = {
      enable = true;
      autoLogin = false;
      defaultUser = "jaga"; 
      theme = pkgs.fetchurl {
          url = https://github.com/jagajaga/nixos-slim-theme/archive/1.1.tar.gz;
          sha256 = "66c3020a6716130a20c3898567339b990fbd7888a3b7bbcb688f6544d1c05c31";
        };
    }; 
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
    config = ''
        Section "InputClass"
            Identifier   "Kensington Slimblade Trackball"
            MatchProduct "Kensington Kensington Slimblade Trackball"
            Option       "ButtonMapping" "1 8 3 4 5 6 7 2 9 10 11 12"
            Option       "EmulateWheel"       "True"
            Option       "EmulateWheelButton" "8"
            Option       "XAxisMapping"       "6 7"
            Option       "ZAxisMapping" "4 5"
            Option       "EmulateWheelInertia" "75"
        EndSection
    '';
  };

  time.timeZone = "Europe/Moscow";

  environment.systemPackages = with pkgs; [
   zsh
   bash
   htop
   iotop

   pmutils
   wget

   gcc
   automake
   pkgconfig
   gnumake
   jdk
   clang
   subversion
   git
   python27
   python33
   python
   cmake

   androidsdk_4_4
   stdenv
   dejavu_fonts

   xsel

   connmanui
   /*cjdns*/

   dropbox
   xlibs.xf86inputjoystick

  ];
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
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
