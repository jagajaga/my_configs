{pkgs}:
let 
  vimrc = import ./vimrc.nix { pkgs = pkgs; };
in
with pkgs; rec {
  setPrio = num: drv: lib.addMetaAttrs { priority = num; } drv;
  haskellPackages = pkgs.haskellPackages;
  lushtags = pkgs.haskell.lib.overrideCabal haskellPackages.lushtags (oldAttrs: {
    src = pkgs.fetchgit {
      url = https://github.com/jagajaga/lushtags;
      rev = "8ec95ca0ffad0f0f8b2e228b2d7d79fc2c7e4a7e";
      sha256 = "15v2f58i1z3421rgxh1i5rbd6cgc4s3vljv1bdjx19b2x9fq237f";
    };
  });

  mocPulse = moc.overrideDerivation (old: { 
    patches = [ 
      ./moc_patches/moc-r2758+pulse_audio-1.patch.gz 
      ./moc_patches/moc-r2758+pulse_audio-1.1.patch.gz 
    ]; 
    preConfigure = ''autoreconf -f -i''; 
    nativeBuildInputs = old.nativeBuildInputs ++ [ libpulseaudio automake libtool autoconf gettext ]; 
  });

  my-pidgin-with-plugins = pkgs.pidgin-with-plugins.override {
    plugins = [ telegram-purple ];
  };

  loveMoc = haskellPackages.callPackage ../myscripts/lovemoc/project.nix { };
  antigen-hs = haskellPackages.callPackage ({ mkDerivation, base, directory, filepath, process, stdenv, text
      }:
      mkDerivation {
        pname = "antigen-hs";
        version = "0.1.0.0";
        src = fetchgit {
          url = https://github.com/Tarrasch/antigen-hs;
          rev = "c089a2b8b394af3c6766808693bd4b6f53167f38";
          sha256 = "0whnf8hqgnmpw4ql7b26j57p5ljw9fs8b15jmqfzgpl5m7jp9clp";
        };
        libraryHaskellDepends = [ base directory filepath process text ];
        homepage = "https://github.com/Tarrasch/antigen-hs";
        description = "A fast zsh plugin manager";
        license = stdenv.lib.licenses.mit;
      }) {};
  /*taffybar = haskellPackages.callPackage ../Dropbox/Programming/Haskell/taffybar/project.nix { }; # until temp fix is not accepted upstream*/

  vimrcConfig = {
        vam.knownPlugins = vimPlugins; # optional
        vam.pluginDictionaries = [
#check ftdetect bug
            { names = [ 
                "YUNOcommit-vim"
                "airline"
                "colors-solarized"
                "ctrlp"
                "easy-align"
                "easymotion"
                "fugitive"
                "ghcmod" 
                "gitgutter"
                "haskellconceal"
                "hoogle"
                "idris-vim"
                "latex-live-preview"
                "lushtags"
                "neco-ghc"
                "nerdcommenter"
                "nerdtree"
                "quickfixstatus"
                "quickrun"
                "rainbow_parentheses"
                "rust"
                "shabadou-vim"
                "signature"
                "surround"
                "syntastic"
                "table-mode"
                "tabmerge"
                "tagbar"
                "taglist"
                "thumbnail-vim"
                "undotree"
                "vim-addon-nix"
                "vim-autoformat"
                "vim-gista"
                "vim-hardtime"
                "vim-hier"
                "vim-racer"
                "vim-wakatime"
                "vim-xkbswitch"
                "vimproc-vim"
                "vundle"
                "watchdogs"
                "webapi-vim"
                "youcompleteme"
                /*"latex-box"*/
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
              astyle
              haskellPackages.stylish-haskell
              my_vim
              racerRust
              rustfmt
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
            chromium
            firefox-bin
            gimp
            /*inkscape*/
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
            my-pidgin-with-plugins
            androidenv.platformTools
            perlPackages.ack
            aspell
            aspellDicts.en
            aspellDicts.ru
            bc
            defaultStdenv
            djview4
            evince
            file
            flac
            freetype
            dejavu_fonts
            gnome.zenity
            gnupg
            gparted
            iftop
            lastfmsubmitd
            libnotify
            lm_sensors
            mocPulse
            mutt
            nethogs
            p7zip
            pass
            pinentry
            psmisc
            python27Packages.goobook
            qutebrowser
            skype
            sxiv
            telnet
            tightvnc
            tmux
            traceroute
            tree
            unrar
            unzip
            usbutils
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

      myHs = haskellPackages.ghcWithPackages (
        pkgs: with pkgs; [ 
            cabal2nix
            cabal-install
            ghc-mod
            hlint
            hoogle
            lushtags
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
            antigen-hs
        ];
      };

      develEnv = lib.lowPrio (
        pkgs.buildEnv {
          name = "development-env";
          ignoreCollisions = true;
          paths = [
              automake
              clang
              cmake
              ctags
              freeglut
              gdb
              gcc
              gnumake
              jdk
              llvm
              manpages
              mesa
              pciutils
              pkgconfig
              python
              python34
              rustUnstable.rustc
              cargo
              smartmontools
              subversion
              swiProlog
              xlibs.libX11
              zlib
          ];
        }
      );
}
