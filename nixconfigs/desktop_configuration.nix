# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let
  literals = import ./literals.nix {pkgs = pkgs;
    caSerokell   = pkgs.writeText "caSerokell.crt" (builtins.readFile /root/.vpn/serokell/ca.crt);
    certSerokell = pkgs.writeText "seniaSerokell.crt" (builtins.readFile /root/.vpn/serokell/senia.crt);
    keySerokell  = pkgs.writeText "seniaSerokell.key" (builtins.readFile /root/.vpn/serokell/senia.key);  
  };
in
{
  require = [
      ./desktop_hardware-configuration.nix
  ];

  imports = [
      ./common_configuration.nix
  ];

  boot.loader.grub.device  = "/dev/sdd";

  /*nixpkgs.config.virtualbox.enableExtensionPack = true;*/

  virtualisation = {

    virtualbox.host.enable = true;

    docker = {
      enable = false;
      storageDriver = "devicemapper";
    };  
  };

  services = {
    # teamviewer.enable = true;
    openvpn = {
      /*servers.Serokell = literals.openVPNConf.configSerokell;*/
    };
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xrandrHeads = [ "DVI-I-0" "HDMI-0" ];
  };

}
