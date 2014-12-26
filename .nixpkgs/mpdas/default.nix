{ stdenv, fetchgit, fetchurl, curl, mpd_clientlib, pkgconfig }:

stdenv.mkDerivation rec {
  name = "mpdas-${version}";
  version = "0.3.2";

  src = fetchgit {
    url = "https://github.com/hrkfdn/mpdas";
    rev = "4b2f62187ad41c4a4d0abf5ff06475c02473df65";
    sha256 = "1gc2xgx74cd5h6zjpzwin7z8v98krrkdf3ny8kz5qnxpafw2cxly";
  };

  patches = [ (fetchurl { url = "https://github.com/hrkfdn/mpdas/pull/14.diff"; md5 = "66065dfe0e8290c8955de822c6f4a37a"; }) ];

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
