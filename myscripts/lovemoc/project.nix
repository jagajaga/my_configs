{ mkDerivation, lens, lens-aeson, liblastfm, libnotify
, process, stdenv, text
}:
mkDerivation {
  pname = "lovemoc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    lens lens-aeson liblastfm libnotify process text
  ];
  license = stdenv.lib.licenses.unfree;
}
