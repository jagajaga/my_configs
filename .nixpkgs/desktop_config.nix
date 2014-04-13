pkgs : {
  allowUnfree = true;
   packageOverrides = self : rec {
      vimEnv = self.buildEnv
      { 
        name = "vim-env";
        paths = with self.vimPlugins;
          [ YouCompleteMe syntastic taglist tagbar vimproc ];
      };

      hsEnv = self.haskellPackages.ghcWithPackagesOld (self : with self; [ cabalInstall_1_18_0_3 cabal2nix xmonad xmobar xmonadContrib AgdaExecutable lushtags haddock stylishHaskell ghcMod hlint ]);

      developmentEnv = self.buildEnv
      {
        name = "development-env";
        paths = with self;
        [ zlib freeglut bzip2 xlibs.libX11 mesa pciutils ctags astyle ];
      };

      hugeEnv = self.buildEnv
      {
        name = "huge-env";
        paths = with self;
        [ gimp libreoffice chromiumWrapper inkscape ];
      };

      deEnv = self.buildEnv
      {
        name = "de-env";
        paths = with self;
        [ skype dropbox haskellPackages.yeganesh dmenu trayer moc transmission_gtk transmission_remote_gtk tor p7zip unrar mc vlc imagemagick spaceFM bc darcs dropbox-cli djview4 xpdf file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage steam steamChrootEnv unetbootin gnome3.gtk lm_sensors mutt python33Packages.glances tightvnc xchat xclip youtubeDL python27Packages.turses dwb gnome.zenity xfce.xfce4notifyd xfce.xfce4terminal libnotify
        vimHugeX irssi ];
      };
  };
}
