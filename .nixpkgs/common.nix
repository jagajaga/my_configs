{pkgs}:
let 
  vimrc = import ./vimrc.nix {};
in
with pkgs; rec {
  setPrio = num: drv: lib.addMetaAttrs { priority = num; } drv;
  ghc-mod = pkgs.haskellngPackages.mkDerivation {
    pname = "ghc-mod";
    version = "5.2.1.2";
    src = pkgs.fetchgit {
      url = "https://github.com/kazu-yamamoto/ghc-mod";
      rev = "247e4e0e7616fe1fecc68fdcf80d6249ac4cee4f";
      sha256 = "0x2llppisw4d6ca8m86by5msphqqp90502bg4hd3a1v91qfjf8ra";
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
  dyreFork = pkgs.haskellngPackages.mkDerivation {
    pname = "dyre";
    version = "0.8.12";
    src = pkgs.fetchgit {
      url = "https://github.com/willdonnelly/dyre";
      rev = "6b371272536469cd269e1de6a0a2c0b136bbcb87";
      sha256 = "1c5m32knjy91lkyji8fz94cxwpjjrqfs7lwh4468n5yr72pvp8yv";
    };
    isLibrary = true;
    buildDepends = with pkgs.haskellngPackages; [
      base binary directory executable-path filepath ghc-paths io-storage
      process time unix xdg-basedir time-compat
    ];
    doCheck = false;
    license = null;
  };
  my_taffybar = haskellngPackages.taffybar.override {
    dyre = dyreFork;
  };
  mocPulse = moc.overrideDerivation (old: { 
    patches = [ 
      ./moc_patches/moc-r2758+pulse_audio-1.patch.gz 
      ./moc_patches/moc-r2758+pulse_audio-1.1.patch.gz 
    ]; 
    preConfigure = ''autoreconf -f -i''; 
    nativeBuildInputs = old.nativeBuildInputs ++ [ pulseaudio automake libtool autoconf gettext ]; 
  });

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
              racerRust
              haskellPackages.stylishHaskell
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
            ghc-mod
            hlint
            hoogle
            /*lushtags*/
            pandoc
            my_taffybar
            xmonad
            xmonad-contrib
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
              automake
              cargoSnapshot
              clang
              cmake
              ctags
              freeglut
              gdb
              gcc
              gnumake
              haskellPackages.idris
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
              rustc
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
