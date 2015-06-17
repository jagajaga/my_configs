# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

{
  require =
    [ 
      ./zenbook_hardware-configuration.nix
#      ./private.nix
    ];

  boot.loader.grub.timeout = 1;

  boot.initrd.kernelModules =
    [ # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];

  boot.kernelPackages      = pkgs.linuxPackages_3_18;
  boot.extraModprobeConfig = ''
      options kvm-amd nested=1
      options kvm-intel nested=1
    '';

  nix.package = pkgs.nixUnstable;
  nix.binaryCaches = [ https://cache.nixos.org https://hydra.nixos.org http://192.168.1.29 ];
  nix.trustedBinaryCaches = [ http://cache.nixos.org https://hydra.nixos.org http://192.168.1.29 ];
  nixpkgs.config.allowUnfree = true;
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.splashImage = ./background.png;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  boot.loader.grub.extraEntries = "menuentry \"Arch Linux\" {\n set root=(hd0,4)\n linux /boot/vmlinuz-linux root=/dev/sda4 ro\n initrd /boot/initramfs-linux.img
    }";

  networking = {
    hostName = "nixosZ"; # Define your hostname.
    connman.enable    = true;
    firewall = {
      allowedTCPPorts = [ 7777 ];
      allowedUDPPorts = [ 7777 ];
    };
    extraHosts = ''fc5d:baa5:61fc:6ffd:9554:67f0:e290:7535 nodeinfo.hype
              fcbf:7bbc:32e4:716:bd00:e936:c927:fc14 socialno.de
              fcd5:76e1:c1c2:e946:b266:8543:c1d5:67ac hypeoverflow.com'';
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

  security.polkit.extraConfig = ''
        polkit.addRule(function(action) {
            if (action.id == "org.freedesktop.udisks2.filesystem-mount-system") {
                return polkit.Result.YES;
            }
        });
  '';

  services.acpid.enable = true; 
  services.acpid.lidEventCommands = ''
    LID="/proc/acpi/button/lid/LID/state"
        state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
        case "$state" in
            *open*) echo $(whoami) > /home/jaga/whoami ;;
            *close*) 
                ${pkgs.su}/bin/su jaga -c ${pkgs.slim}/bin/slimlock &
                systemctl suspend 
                ;;
            *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
        esac
        '';
  
  
  #ask #nixos about dropbox and lidevent

  # List services that you want to enable:
  services.dbus.enable = true;

  services.nixosManual.showManual = true;
  services.locate.enable = true;
  
  # enable automount for media stuff
  services.udisks2.enable = true;
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql;

  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql;
  /*hardware.pulseaudio.enable = true;*/
  /*sound.enableOSSEmulation = false;*/
   /*sound.extraConfig = ''*/
     /*defaults.pcm.!card 3*/
   /*'';*/
  
  # Enable the OpenSSH daemon.
  services.openssh.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.tor.client.enable = true;
  services.upower.enable = true;

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

  services.xserver = {
    exportConfiguration = true;
    enable              = true;
    layout              = "us,ru(winkeys)";
    xkbOptions          = "grp:caps_toggle";
    xkbVariant          = "winkeys";
    displayManager.slim = {
      enable      = true;
      autoLogin   = true;
      defaultUser = "jaga";
      theme = pkgs.fetchurl {
          url    = https://github.com/jagajaga/nixos-slim-theme/archive/1.1.tar.gz;
          sha256 = "66c3020a6716130a20c3898567339b990fbd7888a3b7bbcb688f6544d1c05c31";
        };
    };
    desktopManager = {
      default      = "none";
      xterm.enable = false;
    };
    windowManager = {
      default = "xmonad";
      xmonad = { 
        enable                 = true;
        enableContribAndExtras = true;
      };
    };
    displayManager.sessionCommands = with pkgs; ''
      ${xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr;
      ${coreutils}/bin/sleep 30 && ${dropbox}/bin/dropbox &
      ${networkmanagerapplet}/bin/nm-applet &
      ${feh}/bin/feh --bg-scale ${config.users.extraUsers.jaga.home}/yandex-disk/Camera\ Uploads/etc/fairy_forest_by_arsenixc-d6pqaej.jpg;
      ${lastfmsubmitd}/bin/lastfmsubmitd --no-daemon &
      export BROWSER="dwb";
      exec ${haskellngPackages.xmonad}/bin/xmonad
    '';
    startGnuPGAgent = true;
  };
  programs.ssh.startAgent = false;

  time.timeZone = "Europe/Moscow";

  environment.systemPackages = with pkgs; [
   zsh
   bash
   htop
   iotop
   pmutils
   wget
   git
   mc
   stdenv
   xsel
   connmanui
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
