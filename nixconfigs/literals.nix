{ pkgs, ca ? null, cert ? null, key ? null
, caSerokell ? null, certSerokell ? null, keySerokell ? null
}:
{
  openVPNConf = { 
    configJJ = { 
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
        MIGHAoGBAPC5Q5G3PflAC9fppe9FIPAEjpSZAm2akzO+ttm4VrVxXvImnRkVuf6p
        hoSculyBwuetGrecwE9Kv2jAgYOtLi9iyBe3bjwmixhsp53dYLElNFeoer40JkAB
        tisxLxJbvkMMm/VpNGNedTeOoGy65Njmr+Sx29zA7Dzd9CcgYrDzAgEC
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
    configSerokell = {
      config = ''
        client
        dev tun
        proto udp
        remote memorici.de 10044
        resolv-retry infinite
        nobind
        persist-key
        persist-tun
        ca ${caSerokell}
        cert ${certSerokell}
        key ${keySerokell}
        comp-lzo
        verb 3
      '';
      up = "echo nameserver $nameserver | ''${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
      down = "''${pkgs.openresolv}/sbin/resolvconf -d $dev";
    };
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
        127.0.0.1 local.host
    '';
  workHosts = ''
      192.168.0.9   godzilla
      192.168.0.10  poni
      192.168.0.81  sphinx
      192.168.0.82  alserg
      192.168.0.83  alant
      192.168.0.84  svkazakov
      192.168.0.85  svkazakov-win
      192.168.0.86  melnikov2
      192.168.0.87  tsarev
      192.168.0.89  horse
      192.168.0.108 owl
      192.168.0.78  griffin
      192.168.0.79  hydra
    '';
  sudoConf = ''
    Cmnd_Alias SUSPEND = /var/run/current-system/sw/sbin/pm-suspend, /var/run/current-system/sw/bin/systemctl suspend

    %users      ALL=NOPASSWD: SUSPEND
  '';
  alsaConf = ''
    defaults.pcm.!card 3
  '';

}
