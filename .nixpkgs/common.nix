{pkgs}:
let 
  vimrc = import ./vimrc.nix {};
in
with pkgs; rec {
  vimrcConfig = {
        vam.knownPlugins = vimPlugins; # optional
        vam.pluginDictionaries = [
#check ftdetect bug
            { names = [ 
                "ghcmod" 
                "haskellconceal"
                "hoogle"
                "lushtags"
                "neco-ghc"

                "idris-vim"

                "rust"
                "racer"

                "a"
                "airline"
                "calendar"
                "colors-solarized"
                "ctrlp"
                "easy-align"
                "easymotion"
                "fugitive"
                "gist-vim"
                "gitgutter"
                "hardtime"
                "hier"
                "latex-box"
                "latex-live-preview"
                "nerdcommenter"
                "nerdtree"
                "quickfixstatus"
                "quickrun"
                "rainbow_parentheses"
                "shabadou"
                "signature"
                "surround"
                "table-mode"
                "tabmerge"
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
        customRC = vimrc.config;
      };
        my_vim = vim_configurable.customize { name = "vim"; inherit vimrcConfig; };
      vimEnv = lib.lowPrio (
        buildEnv { 
          name = "vim-env";
          ignoreCollisions = true;
          paths = [
              my_vim
              racerRust
              haskellPackages.stylishHaskell
              astyle
            ];
        }
      );

      emacsEnv = buildEnv {
        name = "emacs-env";
        ignoreCollisions = true;
        paths = [
            emacs
            emacs24Packages.colorThemeSolarized
            emacs24Packages.haskellMode
            emacs24Packages.structuredHaskellMode
        ];
      };

      hugeEnv = buildEnv {
        name = "huge-env";
        ignoreCollisions = true;
        paths = [
            chromiumDev
            gimp
            inkscape
            libreoffice
        ];
      };

      steamEnv = buildEnv {
        name = "steam-env";
        ignoreCollisions = true;
        paths = [
            steam
            steamChrootEnv
        ];
      };

      baseEnv = buildEnv {
        name = "base-env";
        ignoreCollisions = true;
        paths = [ 
            aspell
            aspellDicts.en
            aspellDicts.ru
            bc
            defaultStdenv
            djview4
            dropbox
            dwb
            evince
            file
            flac
            flashplayer
            freetype
            gajim
            dejavu_fonts
            gnome.zenity
            gnupg
            gparted
            iftop
            lastfmsubmitd
            libnotify
            lm_sensors
            mirage
            moc
            mutt
            p7zip
            pass
            pinentry
            skype
            telnet
            termite
            tmux
            traceroute
            tree
            unrar
            unzip
            vlc
            weechat
            which
            wine
            winetricks
            wmname
            xclip
            xlibs.xev
            xlibs.xprop
            xfce.xfce4notifyd
            xfce.xfce4terminal
        ];
      };

      myHs = haskellPackages.ghcWithPackagesOld (
        pkgs: with pkgs; [ 
            /*lushtags*/
            Agda
            cabal2nix
            cabalInstall
            ghcMod
            haddock
            hlint
            hoogle
            hoogleLocal
            taffybar
            xmobar
            xmonad
            xmonadContrib
        ]);

      hsEnv = buildEnv {
        name = "haskell-env";
        ignoreCollisions = true;
        paths = [
            myHs
        ];
      };

      develEnv = lib.lowPrio (
        pkgs.buildEnv {
          name = "development-env";
          ignoreCollisions = true;
          paths = [
              ctags
              /*zlib*/
              automake
              cargoSnapshot
              clang
              cmake
              freeglut
              gcc
              gnumake
              idea.idea-community
              jdk
              manpages
              mercurial
              mesa
              mysql
              mysqlWorkbench
              pciutils
              pkgconfig
              python
              python27
              python33
              ruby
              rustc
              smartmontools
              sqlite
              subversion
              wireshark
              xlibs.libX11
          ];
        }
      );
}
