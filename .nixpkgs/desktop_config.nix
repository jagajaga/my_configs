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
          [ haskellPlatform gtk Chart cairo gloss lens zlib alsaCore alsaPcm ];
      };

      developmentEnv = pkgs.buildEnv
      {
        name = "development-env";
        paths = with pkgs;
        [ zlib freeglut bzip2 xlibs.libX11 mesa ];
      };

      deEnv= pkgs.buildEnv
      {
        name = "de-env";
        paths = with pkgs;
        [ haskellPackages.xmobar skype dropbox haskellPackages.yeganesh dmenu trayer moc gimp libreoffice transmission_gtk transmission_remote_gtk firefoxWrapper tor chromiumDevWrapper p7zip unrar mc vlc imagemagick spaceFM ];
      };

   };
 }
