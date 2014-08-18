pkgs : {
  allowUnfree = true;
   packageOverrides = self : rec {
      vimEnv = self.buildEnv
      { 
        name = "vim-env";
        paths = with self.vimPlugins;
          [ YouCompleteMe syntastic taglist tagbar vimproc ];
      };

      hsEnv = self.haskellPackages.ghcWithPackagesOld (self : with self; [ cabalInstall_1_18_0_3 cabal2nix lushtags haddock stylishHaskell ghcMod hlint ]);

      developmentEnv = self.buildEnv
      {
        name = "development-env";
        paths = with self;
        [ zlib freeglut bzip2 xlibs.libX11 mesa pciutils astyle manpages emacs ];
      };

      hugeEnv = self.buildEnv
      {
        name = "huge-env";
        paths = with self;
        [ chromiumWrapper ];
      };

      deEnv = self.buildEnv
      {
        name = "de-env";
        paths = with self;
        [ skype gajim steam steamChrootEnv dmenu trayer moc transmission_gtk transmission_remote_gtk tor p7zip unrar mc vlc imagemagick bc djview4 evince xfe file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage  unetbootin lm_sensors mutt tightvnc xclip python27Packages.turses gnome.zenity xfce.xfce4notifyd xfce.xfce4terminal libnotify weechat dwb dropbox psmisc ];
      };
      
  };
}
