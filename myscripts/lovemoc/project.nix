{ cabal, liblastfm, process, lensAeson, lens, libnotify }:

cabal.mkDerivation (self: {
  pname = "lovemoc";
  version = "0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    liblastfm process lensAeson lens libnotify
  ];
})
