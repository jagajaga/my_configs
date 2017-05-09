# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:
let
  literals = import ./literals.nix {
    pkgs = pkgs;
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
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xrandrHeads = [ "DVI-I-0" "HDMI-0" ];
  };

}
