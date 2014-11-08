{
  allowUnfree = true;
  allowBroken = true;

  packageOverrides = pkgs : with pkgs; rec {
        /*vimrcConfig = {*/
        /*vam.knownPlugins = pkgs.vimPlugins; # optional*/
        /*vam.pluginDictionaries = [*/
            /*{names = ["ghcmod" "syntastic" "youcompleteme" "Solarized" ];}*/
        /*];*/

        /*customRC = ''*/
            /*set hidden*/
        /*'';*/
        /*};*/
    /*testing_vim = pkgs.vim_configurable.customize { name = "vim-with-plugins"; inherit vimrcConfig; }; */
      vimEnv = pkgs.buildEnv
      { 
        name = "vim-env";
        paths = with pkgs.vimPlugins;
          [ youcompleteme taglist tagbar vundle fugitive nerdtree airline ghcmod-vim neco-ghc a nerdcommenter undotree easymotion colors-solarized table-mode vimproc tagbar haskellconceal hoogle gist-vim webapi-vim lushtags calendar thumbnail latex-live-preview latex-box easy-align gitgutter hardtime tabmerge rainbow_parentheses rust idris-vim quickfixstatus  hier shabadou quickrun watchdogs signature ];
          #surround
      };

      hsEnv = pkgs.haskellPackages.ghcWithPackagesOld (pkgs: with pkgs; [ cabalInstall cabal2nix xmonad xmobar xmonadContrib Agda lushtags haddock stylishHaskell ghcMod hlint hoogle hoogleLocal ]);

      developmentEnv = pkgs.buildEnv
      {
        name = "development-env";
        paths = [ zlib freeglut bzip2 xlibs.libX11 mesa pciutils astyle manpages ctags ];
      };

      hugeEnv = pkgs.buildEnv
      {
        name = "huge-env";
        paths = [ gimp libreoffice chromiumDev inkscape ];
      };

      emacsEnv = pkgs.buildEnv
      {
        name = "emacs-env";
        paths = [ emacs emacs24Packages.haskellMode emacs24Packages.colorThemeSolarized emacs24Packages.structuredHaskellMode ];
      };

      /*vvEnv = pkgs.buildEnv */
      /*{*/
        /*name = "vv-env";*/
        /*paths =  */
        /*let */
            /*vimrcConfig = {*/
            /*vam.pluginDictionaries = [*/
                /*# load always*/
                /*{name = "youcompleteme";}*/
            /*];*/

                /*customRC = ''*/
                    /*set hidden*/
                /*'';*/
                /*};*/
            /*test_vim_with_vim_addon_nix_using_vam = vim_configurable.customize { name = "vim-with-plugins"; inherit vimrcConfig; }; */
        /*in*/
        /*[test_vim_with_vim_addon_nix_using_vam];*/
      /*};*/

      deEnv = pkgs.buildEnv
      {
        name = "de-env";
        paths = [ trayer moc transmission_gtk transmission_remote_gtk tor p7zip unrar mc vlc imagemagick bc darcs djview4 evince xfe steam steamChrootEnv file flac freetype gtkvnc hdparm iftop lastfmsubmitd mirage  unetbootin lm_sensors mutt python33Packages.glances tightvnc xclip youtubeDL python27Packages.turses gnome.zenity xfce.xfce4notifyd xfce.xfce4terminal libnotify weechat aspell aspellDicts.ru aspellDicts.en dwb ];
      };
  };
}
