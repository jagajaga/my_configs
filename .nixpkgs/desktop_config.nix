pkgs : {
  allowUnfree = true;
   packageOverrides = self : rec {
      vimEnv = self.buildEnv
      { 
        name = "vim-env";
        paths = with self.vimPlugins;
          [ YouCompleteMe syntastic taglist tagbar vimproc ];
      };

      hsEnv = self.haskellPackages.ghcWithPackagesOld (self : with self; [ cabalInstall_1_18_0_3 cabal2nix xmonad xmobar xmonadContrib Agda lushtags haddock stylishHaskell ghcMod hlint ]);

      developmentEnv = self.buildEnv
      {
        name = "development-env";
        paths = with self;
        [ zlib freeglut bzip2 xlibs.libX11 mesa pciutils ctags astyle manpages ];
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
        [ skype dropbox dmenu trayer moc transmission_gtk transmission_remote_gtk tor p7zip unrar mc vlc imagemagick bc darcs djview4 evince xfe steam steamChrootEnv file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage  unetbootin lm_sensors mutt python33Packages.glances tightvnc xclip youtubeDL python27Packages.turses gnome.zenity xfce.xfce4notifyd xfce.xfce4terminal libnotify vimHugeX weechat aspell aspellDicts.ru aspellDicts.en dwb ];
      };
  };
}
