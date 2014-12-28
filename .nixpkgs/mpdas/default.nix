{ stdenv, fetchgit, fetchurl, curl, mpd_clientlib, pkgconfig }:

stdenv.mkDerivation rec {
  name = "mpdas-${version}";
  version = "0.4-alpha";

  src = fetchgit {
    url = "https://github.com/hrkfdn/mpdas";
    rev = "8900f5b437109ac8fd27b95330938abec47aa246";
    sha256 = "1l4hfmcp1l0a08cjf3lmvnlksagrkpvnvfbhdlzziva5dddprs31";
  };

  buildInputs = [ curl mpd_clientlib pkgconfig ];

  installPhase = ''
    install -d $out/bin
    install -d $out 
    install -m 755 mpdas $out/bin
    install -m 644 mpdas.1 $out/mpdas.1
  '';

  meta = {
    homepage = "http://50hz.ws/mpdas/";
    description = "A C++ client to submit tracks to audioscrobbler, supports new protocol";
  };
}
