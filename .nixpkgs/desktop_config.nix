pkgs : {
  allowUnfree = true;
  allowBroken = true; 
  firefox.enableGoogleTalkPlugin = true;
  firefox.enableEsteid = true;
  /*allowBroken = true;*/
  packageOverrides = pkgs : with pkgs; rec {
      common = import ./common.nix { pkgs = pkgs; }; 
      inherit (common) vimEnv hsEnv hugeEnv emacsEnv baseEnv develEnv steamEnv mocPulse;
      desktopEnv = pkgs.buildEnv
      {
        name = "desktop-env";
        ignoreCollisions = true;
        paths = [
            /*calibre*/
            google-musicmanager
            /*gtkvnc*/
            /*teamviewer*/
            /*texLiveFull*/
            /*viber*/
            desktop_file_utils
            e2fsprogs
            electrum
            ffmpeg
            fpc
            gnome3.cheese
            gnome3.gnome-video-effects
            graphviz
            guvcview
            hdparm
            imagemagick
            inkscape
            jmtpfs
            kde4.k3b
            lshw
            lsof
            mbox
            megatools
            mplayer
            nix-prefetch-scripts
            nix-repl
            nox
            /*octave*/
            openvpn
            parted
            pastebinit
            pavucontrol
            pdftk
            perlPackages.FileMimeInfo
            psmisc
            pwgen
            python27Packages.pytz
            python27Packages.turses
            python33Packages.glances
            python34Packages.rainbowstream
            qemu
            shared_mime_info
            spaceFM
            /*spotify*/
            tightvnc
            tor
            transmission_gtk
            transmission_remote_gtk
            unetbootin
            xlibs.xf86inputjoystick
        ];
      };
  };
}

