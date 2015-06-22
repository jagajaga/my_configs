# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let
  literals = import ./literals.nix {pkgs = pkgs;
                ca   = pkgs.writeText "ca.crt" (builtins.readFile /root/.vpn/ca.crt);
                cert = pkgs.writeText "alice" (builtins.readFile /root/.vpn/alice.crt);
                key  = pkgs.writeText "alice.key" (builtins.readFile /root/.vpn/alice.key);  
  };
in

{
  require = [
      ./private.nix
  ];

  boot = {
    kernelPackages      = pkgs.linuxPackages_3_19;
    extraModprobeConfig = ''
      options snd slots=snd_usb_audio,snd-hda-intel
      options kvm-amd nested=1
      options kvm-intel nested=1
    '';
    cleanTmpDir = true;
    loader.grub = {
      timeout = 1;
      enable  = true;
      version = 2;
    };
  };

  nix = {
    /*package             = pkgs.nixUnstable;*/
    binaryCaches = [ https://cache.nixos.org https://hydra.nixos.org ];
    trustedBinaryCaches = [ https://cache.nixos.org https://hydra.nixos.org http://hydra.cryp.to ];
    useChroot           = true;
    gc = {
      automatic = true;
      dates     = "00:00";
    };
  };

  nixpkgs.config = {
    allowUnfree                    = true;
  };

  networking = {
    extraHosts               = literals.extraHosts;
    networkmanager.enable    = true;
  };

  i18n = {
    consoleFont   = "lat9w-16";
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    opengl = {
      driSupport32Bit = true;
    };
    pulseaudio = {
      enable = true;
    };
  };

  security.sudo.configFile = literals.sudoConf;

  services = {
    cron.systemCronJobs = [
       "20 * * * * jaga rsync --remove-source-files -rauz ${config.users.extraUsers.jaga.home}/{Dropbox/\"Camera Uploads\",yandex-disk/}"
       "22 * * * * root systemctl restart yandex-disk.service"
    ];
    dbus.enable            = true;
    nixosManual.showManual = true;
    nix-serve.enable       = true;
    journald.extraConfig   = "SystemMaxUse=50M";
    locate.enable          = true;
    udisks2.enable         = true;
    openssh.enable         = true;
    openntpd.enable        = true;
    openvpn = {
      enable         = true;
      servers.client = literals.openVPNConf;
    };
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
      export BROWSER="dwb";
      exec ${haskellngPackages.xmonad}/bin/xmonad
    '';
    config = literals.trackBallConf;
    startGnuPGAgent = true;
  };

  systemd.services.lastfmsubmitd = {
    wantedBy = [ "multi-user.target" ]; 
    after = [ "network.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = ''${pkgs.screen}/bin/screen -dmS lastfsubmit ${pkgs.lastfmsubmitd}/bin/lastfmsubmitd --no-daemon'';
      ExecReload = ''${pkgs.screen}/bin/screen -S lastfsubmit -X quit'';
    };
  };

  programs.ssh.startAgent = false;

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
   dropbox
   git
   htop
   iotop
   mc
   networkmanager
   networkmanagerapplet
   pmutils
   stdenv
   wget
   xsel
   zsh
  ];
  fonts = {
    fontconfig.enable = true;
    enableFontDir          = true;
    enableGhostscriptFonts = true;
    fontconfig.defaultFonts.monospace = ["Terminus"];
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
