pkgs : {
  allowUnfree = true;
  allowBroken = true; 
  /*allowBroken = true;*/
  packageOverrides = pkgs : with pkgs; rec {
      teamviewer = pkgs.teamviewer.override {
        acceptLicense = true;
      };
      common = import ./common.nix { pkgs = pkgs; }; 
      inherit (common) vimEnv hsEnv hugeEnv emacsEnv baseEnv develEnv steamEnv mocPulse;
      desktopEnv = pkgs.buildEnv
      {
        name = "desktop-env";
        ignoreCollisions = true;
        paths = [
            bittorrentSync
            /*calibre*/
            darcs
            desktop_file_utils
            dmg2img
            dvdplusrwtools
            e2fsprogs
            ffmpeg
            fpc
            /*google-musicmanager*/
            graphviz
            /*gtkvnc*/
            guvcview
            hdparm
            imagemagick
            jmtpfs
            lshw
            lsof
            mbox
            megatools
            mplayer
            nix-prefetch-scripts
            nix-repl
            nox
            octave
            openvpn
            pastebinit
            parted
            pavucontrol
            perlPackages.FileMimeInfo
            psmisc
            pwgen
            python27Packages.pytz
            python27Packages.turses
            python33Packages.glances
            python34Packages.rainbowstream
            qemu
            spaceFM
            spotify
            shared_mime_info
            teamviewer
            /*texLiveFull*/
            tightvnc
            tor
            transmission_gtk
            transmission_remote_gtk
            unetbootin
            /*viber*/
            xlibs.xf86inputjoystick
        ];
      };
  };
}

