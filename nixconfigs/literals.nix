{pkgs, ca ? null, cert ? null, key ? null}:
{
  openVPNConf = { 
      config = ''
            client
            nobind
            dev tun
            redirect-gateway def1
            ca ${ca}
            cert ${cert}
            key ${key}
            <dh>
            -----BEGIN DH PARAMETERS-----
            MEYCQQCQn3sGfqQWQtMH4GNWHeZG6210sE3cssDGitRfv9T9knp00zIquPI3tuRa
            xywN8CG+Ww/V8kBIgLBfqRqnThdzAgEC
            -----END DH PARAMETERS-----
            </dh>

            <connection>
            remote 178.62.202.50 1194 udp
            </connection>

            <connection>
            remote 178.62.202.50 443 tcp-client
            </connection>
    '';
    up = "echo nameserver $nameserver | ''${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
    down = "''${pkgs.openresolv}/sbin/resolvconf -d $dev";
  };
  trackBallConf = ''
        Section "InputClass"
            Identifier   "Kensington Slimblade Trackball"
            MatchProduct "Kensington Kensington Slimblade Trackball"
            Option       "ButtonMapping" "1 8 3 4 5 6 7 2 9 10 11 12"
            Option       "EmulateWheel"       "True"
            Option       "EmulateWheelButton" "8"
            Option       "XAxisMapping"       "6 7"
            Option       "ZAxisMapping" "4 5"
            Option       "EmulateWheelInertia" "75"
        EndSection
    '';
  extraHosts = ''
        fc5d:baa5:61fc:6ffd:9554:67f0:e290:7535 nodeinfo.hype
        fcbf:7bbc:32e4:716:bd00:e936:c927:fc14 socialno.de
        fcd5:76e1:c1c2:e946:b266:8543:c1d5:67ac hypeoverflow.com
    '';
  sudoConf = ''
    Cmnd_Alias SUSPEND = /var/run/current-system/sw/sbin/pm-suspend, /var/run/current-system/sw/bin/systemctl suspend

    %users      ALL=NOPASSWD: SUSPEND
  '';
  alsaConf = ''
    defaults.pcm.!card 3
  '';

}
