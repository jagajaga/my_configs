pkgs : {
  allowUnfree = true;
  /*allowBroken = true;*/
  packageOverrides = pkgs : with pkgs; rec {
      common = import ./common.nix { pkgs = pkgs; }; 
      inherit (common) vimEnv hsEnv hugeEnv emacsEnv baseEnv develEnv steamEnv;
      desktopEnv = pkgs.buildEnv
      {
        name = "desktop-env";
        ignoreCollisions = true;
        paths = [
            bittorrentSync
            calibre
            darcs
            desktop_file_utils
            dmg2img
            dvdplusrwtools
            e2fsprogs
            ffmpeg
            fish
            fpc
            google-musicmanager
            graphviz
            gtkvnc
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
            pavucontrol
            perlPackages.FileMimeInfo
            psmisc
            pwgen
            python27Packages.pytz
            python27Packages.turses
            python33Packages.glances
            qemu
            shared_mime_info
            teamviewer
            texLiveFull
            /*tightvnc*/
            tor
            transmission_gtk
            transmission_remote_gtk
            unetbootin
            xfe
            xlibs.xf86inputjoystick
            youtubeDL
        ];
      };
  };
}

