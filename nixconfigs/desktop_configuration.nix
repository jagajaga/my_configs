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

  boot.kernelPackages = pkgs.linuxPackages_3_13;
  boot.loader.grub.timeout = 1;

  boot.initrd.kernelModules =
    [ # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];

  boot.extraModprobeConfig = ''
    options snd slots=snd_usb_audio,snd-hda-intel

  '';
    /*options snd_usb_audio index=0*/
    /*options snd_hda_intel index=1*/
    
  nix.package = pkgs.nixUnstable;
  nix.binaryCaches = [ http://cache.nixos.org ];
  nix.trustedBinaryCaches = [ http://cache.nixos.org ];
  nixpkgs.config.allowUnfree = true;
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdd";

  boot.loader.grub.extraEntries = "menuentry \"Arch Linux\" {\n set root=(hd0,1)\n linux /boot/vmlinuz-linux root=/dev/sdc1 ro\n initrd /boot/initramfs-linux.img
  }";

  networking = {
    hostName = "nixos"; # Define your hostname.
    connman.enable = true;
    /*interfaceMonitor.enable = false;*/
    /*wireless.enable = false; # Don't run wpa_supplicant (wicd will do it when necessary)*/
    /*useDHCP = false; # Don't run dhclient on wlan0*/
    /*wicd.enable = true;*/
    extraHosts = ''fc5d:baa5:61fc:6ffd:9554:67f0:e290:7535 nodeinfo.hype
              fcbf:7bbc:32e4:716:bd00:e936:c927:fc14 socialno.de
              fcd5:76e1:c1c2:e946:b266:8543:c1d5:67ac hypeoverflow.com'';
  };


  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.


  fileSystems."/home" =     # where you want to mount the device
    { device = "/dev/sdd2";  # the device
      fsType = "ext4";      # the type of the partition
      options = "data=journal,users,rw,user,auto,exec";
    };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  security.sudo.configFile = ''
    Cmnd_Alias SUSPEND = /var/run/current-system/sw/sbin/pm-suspend, /var/run/current-system/sw/bin/systemctl suspend

    %users      ALL=NOPASSWD: SUSPEND
  '';

  # List services that you want to enable:
  services.dbus.enable = true;

  services.nixosManual.showManual = true;
  services.locate.enable = true;
  
  # enable automount for media stuff
  services.udisks2.enable = true;

  /*hardware.pulseaudio.enable = true;*/
  /*sound.enableOSSEmulation = false;*/
  sound.extraConfig = ''
    defaults.pcm.!card 3
  '';
  
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.tor.client.enable = true;
  services.cjdns.enable = true;


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

  # Enable the X11 windowing system.
  services.xserver = {
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
        url = https://github.com/jagajaga/nixos-slim-theme/archive/1.0.tar.gz;
        sha256 = "08ygjn5vhn3iavh36pdcb15ij3z34qnxp20xh3s1hy2hrp63s6kn";
      };
    }; 
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
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

   androidsdk_4_1 #todo
   stdenv
   dejavu_fonts

   xsel

   connmanui
   cjdns

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
