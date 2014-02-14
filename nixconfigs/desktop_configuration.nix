
# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

let mySlimTheme = pkgs.fetchurl {
      url = https://github.com/jagajaga/nixos-slim-theme/raw/master/nixos-slim-theme.tar.gz;
      sha256 = "62da019ccf69be29cb1f75944628d01badb792807ff4f15da5d0f2aaeba8c72e";
    }; 
in

{
  require =
    [ 
      ./desktop_hardware-configuration.nix
      ./private.nix
    ];

  boot.initrd.kernelModules =
    [ # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];

  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
    
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdd";

  boot.loader.grub.extraEntries = "menuentry \"Arch Linux\" {\n set root=(hd0,1)\n linux /boot/vmlinuz-linux root=/dev/sdc1 ro\n initrd /boot/initramfs-linux.img
  }";

  networking = {
    hostName = "nixos"; # Define your hostname.
    interfaceMonitor.enable = false;
    wireless.enable = false; # Don't run wpa_supplicant (wicd will do it when necessary)
    useDHCP = false; # Don't run dhclient on wlan0
    wicd.enable = true;
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
  services.udisks.enable = true;

  /*hardware.pulseaudio.enable = true;*/
  
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.tor.client.enable = true;


  users.extraUsers.jaga = {
    description = "";
    createHome = true;
    home = "/home/jaga";
    group = "users";
    extraGroups = [ "wheel"];
    shell = "${pkgs.zsh}/bin/zsh";
    uid = 1000;
  };

  services.mesa = {
    videoDrivers = [ "nvidia" ];
    driSupport32Bit = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us,ru(winkeys)";
    xkbOptions = "grp:caps_toggle";
    xkbVariant = "winkeys";
    displayManager.slim = {
      enable = true;
      autoLogin = true;
      defaultUser = "jaga"; 
      theme = mySlimTheme;      
      package = pkgs.slim.override { 
        theme = mySlimTheme; 
      };
    };
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
  };

  time.timeZone = "Europe/Moscow";


  /*nixpkgs.config.cabal.libraryProfiling = true;*/

  environment.systemPackages = with pkgs; [
   zsh
   bash
   htop
   iotop
   gnome.zenity
   xfce.xfce4notifyd
   xfce.xfce4terminal
   libnotify

   pmutils
   wget

   gcc
   automake
   pkgconfig
   gnumake
   qt5Full
   boost
   jdk
   clang
   subversion
   git
   python27
   python33
   python
   cmake
   haskellPackages.cabalInstall_1_18_0_2 

   androidsdk_4_1 #todo
   stdenv
   dejavu_fonts

   /*emacs24*/
   /*emacs24Packages.haskellMode*/
   vimHugeX

    xsel

  ];
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    extraFonts = [
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
