{ cabal, parsec, process }:

cabal.mkDerivation (self: {
  pname = "git-prompt";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    parsec process
  ];
})
