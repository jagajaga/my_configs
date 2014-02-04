 {
   packageOverrides = pkgs : with pkgs; {
      vimEnv = pkgs.buildEnv
      { name = "vim-env";
        paths = with pkgs.vimPlugins;
          [ YouCompleteMe syntastic taglist tagbar vimproc ctags ];
      };

      hsEnv = pkgs.buildEnv
      { name = "hs-env";
        paths = with pkgs.haskellPackages;
          [ haskellPlatform gtk Chart cairo gloss lens zlib alsaCore alsaPcm cabal2nix ];
      };

      developmentEnv = pkgs.buildEnv
      {
        name = "development-env";
        paths = with pkgs;
        [ zlib freeglut bzip2 xlibs.libX11 mesa ];
      };

      hugeEnv = pkgs.buildEnv
      {
        name = "huge-env";
        paths = with pkgs;
        [ gimp libreoffice chromiumDevWrapper ];
      };

      deEnv = pkgs.buildEnv
      {
        name = "de-env";
        paths = with pkgs;
        [ haskellPackages.xmobar skype dropbox haskellPackages.yeganesh dmenu trayer moc transmission_gtk transmission_remote_gtk firefoxWrapper tor p7zip unrar mc vlc imagemagick spaceFM bc darcs dropbox-cli evince file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage  steam steamChrootEnv unetbootin ];
      };

   };
 }
