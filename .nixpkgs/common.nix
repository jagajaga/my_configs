{pkgs}:
let 
  vimrc = import ./vimrc.nix {};
in
with pkgs; rec {
  setPrio = num: drv: lib.addMetaAttrs { priority = num; } drv;

  ghc-mod = pkgs.haskellngPackages.mkDerivation {
    pname = "ghc-mod";
    version = "5.2.1.3";
    src = pkgs.fetchgit {
      url = "https://github.com/kazu-yamamoto/ghc-mod";
      rev = "ea03f8a935d82cfd4a5af58dd5be5ef6e4741dcc";
      sha256 = "1anx871dib9gykzf7xfpksywn4qqdkxfzmklxk11ggjhxgapbwaq";
    };
    isLibrary = false;
    isExecutable = true;
    buildDepends = with haskellngPackages; [
      async base Cabal containers data-default deepseq directory
      djinn-ghc filepath ghc ghc-paths ghc-syb-utils haskell-src-exts
      hlint io-choice monad-control monad-journal mtl old-time pretty
      process split syb temporary text time transformers
      transformers-base cabal-helper cereal
    ];
    doCheck = false;
    homepage = "http://www.mew.org/~kazu/proj/ghc-mod/";
    description = "Happy Haskell Programming";
    license = stdenv.lib.licenses.bsd3;
  };

  mocPulse = moc.overrideDerivation (old: { 
    patches = [ 
      ./moc_patches/moc-r2758+pulse_audio-1.patch.gz 
      ./moc_patches/moc-r2758+pulse_audio-1.1.patch.gz 
    ]; 
    preConfigure = ''autoreconf -f -i''; 
    nativeBuildInputs = old.nativeBuildInputs ++ [ pulseaudio automake libtool autoconf gettext ]; 
  });

  loveMoc = pkgs.haskellngPackages.callPackage ../myscripts/lovemoc/project.nix { };

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
                /*"racer"*/

                "a"
                "airline"
                "colors-solarized"
                "ctrlp"
                "easy-align"
                "easymotion"
                "fugitive"
                "gitgutter"
                "hardtime"
                "hier"
                /*"latex-box"*/
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
                "vim-gista"
                "wakatime"
                "watchdogs"
                "webapi-vim"
                "xkbswitch"
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
              /*racerRust*/
              haskellngPackages.stylish-haskell
              astyle
            ];
        }
      );

      emacsEnv = setPrio "9" (
        buildEnv {
          name = "emacs-env";
          ignoreCollisions = true;
          paths = [
          (emacsWithPackages (with emacsPackages; with emacsPackagesNg; [
              company
              company-ghc
              evil
              evil-leader
              #evil-surround
              flycheck
              haskell-mode
              helm
              markdown-mode
              monokai-theme
              org
              rainbow-delimiters
              undo-tree
              use-package
            ]))
          ];
        }
      );

      hugeEnv = buildEnv {
        name = "huge-env";
        ignoreCollisions = true;
        paths = [
            chromiumDev
            firefoxWrapper
            gimp
            /*inkscape*/
            /*libreoffice*/
        ];
      };

      steamEnv = buildEnv {
        name = "steam-env";
        ignoreCollisions = true;
        paths = [
            steam
        ];
      };

      baseEnv = lib.lowPrio (
        buildEnv {
          name = "base-env";
          ignoreCollisions = true;
          paths = [ 
            perlPackages.ack
            aspell
            aspellDicts.en
            aspellDicts.ru
            bc
            defaultStdenv
            djview4
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
            mocPulse
            mutt-with-sidebar 
            p7zip
            pass
            pinentry
            psmisc
            skype
            sxiv
            telnet
            tightvnc
            tmux
            traceroute
            tree
            unrar
            unzip
            vlc
            weechat
            which
            wineUnstable
            winetricks
            wmname
            xclip
            xkblayout-state
            xlibs.xev
            xlibs.xprop
            xfce.xfce4notifyd
            xfce.xfce4terminal
            zip
          ];
        }
      );

      myHs = haskellngPackages.ghcWithPackages (
        pkgs: with pkgs; [ 
            cabal2nix
            cabal-install
            /*ghc-mod*/
            hlint
            hoogle
            /*lushtags*/
            pandoc
            taffybar
            xmonad
            xmonad-contrib
        ]);

      hsEnv = buildEnv {
        name = "haskell-env";
        ignoreCollisions = true;
        paths = [
            myHs
            loveMoc
        ];
      };

      develEnv = lib.lowPrio (
        pkgs.buildEnv {
          name = "development-env";
          ignoreCollisions = true;
          paths = [
              automake
              cargoSnapshot
              clang
              cmake
              ctags
              freeglut
              gdb
              gcc
              gnumake
              /*haskellngPackages.idris*/
              idea.idea-community
              jdk
              manpages
              mercurial
              mesa
              pciutils
              pkgconfig
              python
              python34
              ruby
              /*rustc*/
              smartmontools
              sqlite
              subversion
              swiProlog
              wireshark
              xlibs.libX11
              zlib
          ];
        }
      );
}
