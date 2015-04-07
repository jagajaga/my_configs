{ mkDerivation, base, parsec, process, stdenv }:
mkDerivation {
  pname = "git-prompt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base parsec process ];
  homepage = "https://github.com/olivierverdier/zsh-git-prompt";
  description = "Informative git prompt for zsh";
  license = stdenv.lib.licenses.mit;
}

