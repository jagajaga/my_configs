# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

{
  imports = [ <nixos/modules/programs/virtualbox.nix> ];
  require =
    [ 
      ./desktop_hardware-configuration.nix
      ./private.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_3_16;
  boot.loader.grub.timeout = 1;
  boot.extraModprobeConfig = ''
      options snd slots=snd_usb_audio,snd-hda-intel
  '';

  nix.package = pkgs.nixUnstable;
  nix.binaryCaches = [ http://cache.nixos.org ];
  nix.trustedBinaryCaches = [ http://cache.nixos.org ];
  nixpkgs.config.allowUnfree = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sdc";
  /*boot.loader.grub.extraEntries = "menuentry \"Arch Linux\" {\n set root=(hd0,1)\n linux /boot/vmlinuz-linux root=/dev/sdc1 ro\n initrd /boot/initramfs-linux.img}";*/

  networking = {
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
  services.tor.client.enable = true;
  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql;
  /*services.cjdns.enable = true;*/

  sound.extraConfig = ''
    defaults.pcm.!card 3
  '';
  


  users.extraUsers.jaga = {
    description = "";
    createHome = true;
    home = "/home/jaga";
    group = "users";
    extraGroups = [ "wheel" "networkmanager" "adb" ];
    shell = "${pkgs.zsh}/bin/zsh";
    uid = 1000;
  };

  hardware.opengl = {
    driSupport32Bit = true;
  };

  hardware.bluetooth.enable = true;

  services.xserver = {
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

   androidsdk_4_4 #todo
   stdenv
   dejavu_fonts

   xsel

   connmanui
   /*cjdns*/

   dropbox

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
