{
  allowUnfree = true;
  /*allowBroken = true;*/

  packageOverrides = pkgs : with pkgs; rec {
        vimrcConfig = {
        vam.knownPlugins = pkgs.vimPlugins; # optional
        vam.pluginDictionaries = [
            {names = [ 
                "a"
                "airline"
                "calendar"
                "colors-solarized"
                "easy-align"
                "easymotion"
                "fugitive"
                "ghcmod"
                "gist-vim"
                "gitgutter"
                "hardtime"
                "haskellconceal"
                "hier"
                "hoogle"
                "idris-vim"
                "latex-box"
                "latex-live-preview"
                "lushtags"
                "neco-ghc"
                "nerdcommenter"
                "nerdtree"
                "quickfixstatus"
                "quickrun"
                "rainbow_parentheses"
                "rust"
                "shabadou"
                "signature"
                "surround"
                "table-mode"
                "tabmerge"
                "tagbar"
                "tagbar"
                "taglist"
                "thumbnail"
                "undotree"
                "vimproc"
                "vim-snippets"
                "vundle"
                "watchdogs"
                "webapi-vim"
                "YUNOcommit"
                "youcompleteme"
            ];}
        ];

        customRC = ''
            set hidden
            exec 'source '.fnameescape($HOME.'/.vimrc')
        '';
        };
        my_vim = pkgs.vim_configurable.customize { name = "vim"; inherit vimrcConfig; };
      vimEnv = pkgs.buildEnv
      { 
        name = "vim-env";
        paths = with pkgs;
          [
            my_vim
          ];
      };

      hsEnv = pkgs.haskellPackages.ghcWithPackagesOld (pkgs: with pkgs; [ 
            /*lushtags*/
            Agda
            cabal2nix
            cabalInstall
            ghcMod
            haddock
            hlint
            hoogle
            hoogleLocal
            stylishHaskell
            xmobar
            xmonad
            xmonadContrib
        ]);

      developmentEnv = pkgs.buildEnv
      {
        name = "development-env";
        paths = [
            astyle
            bzip2
            ctags
            freeglut
            manpages
            mesa
            pciutils
            xlibs.libX11
            zlib
        ];
      };

      hugeEnv = pkgs.buildEnv
      {
        name = "huge-env";
        paths = [
            chromiumDev
            gimp
            inkscape
            libreoffice
        ];
      };

      emacsEnv = pkgs.buildEnv
      {
        name = "emacs-env";
        paths = [
            emacs
            emacs24Packages.colorThemeSolarized
            emacs24Packages.haskellMode
            emacs24Packages.structuredHaskellMode
        ];
      };

      deEnv = pkgs.buildEnv
      {
        name = "de-env";
        paths = [ 
            aspell
            aspellDicts.en
            aspellDicts.ru
            bc
            darcs
            djview4
            dwb
            evince
            file
            flac
            freetype
            gnome.zenity
            gtkvnc
            hdparm
            iftop
            imagemagick
            lastfmsubmitd
            libnotify
            lm_sensors
            mc
            mirage
            moc
            mutt
            p7zip
            python27Packages.turses
            python33Packages.glances
            steam
            steamChrootEnv
            tightvnc
            tor
            transmission_gtk
            transmission_remote_gtk
            trayer
            unetbootin
            unrar
            vlc
            weechat
            xclip
            xfce.xfce4notifyd
            xfce.xfce4terminal
            xfe
            youtubeDL
        ];
      };
  };
}
