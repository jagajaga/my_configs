pkgs : {
   packageOverrides = self : rec {
      vimEnv = self.buildEnv
      { 
        name = "vim-env";
        paths = with self.vimPlugins;
          [ YouCompleteMe syntastic taglist tagbar vimproc ctags ];
      };

      hsEnv = self.haskellPackages.ghcWithPackagesOld (self : with self; [ ghc gtk Chart cairo gloss lens zlib alsaCore alsaPcm cabal2nix pandoc hakyll ghcMod unorderedContainers xmlConduit xmonad xmobar xmonadContrib AgdaExecutable haddock haskellSrcExts stylishHaskell aeson mtlparse regexpr ]);
#lushtags 

      developmentEnv = self.buildEnv
      {
        name = "development-env";
        paths = with self;
        [ zlib freeglut bzip2 xlibs.libX11 mesa_drivers mesa_glu ];
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
        [ skype dropbox haskellPackages.yeganesh dmenu trayer moc transmission_gtk transmission_remote_gtk firefoxWrapper tor p7zip unrar mc vlc imagemagick spaceFM bc darcs dropbox-cli evince file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage steam steamChrootEnv unetbootin dwb ];
      };
  };
}
