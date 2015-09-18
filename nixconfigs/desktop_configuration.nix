# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

{
  require = [
      ./desktop_hardware-configuration.nix
  ];

  imports = [
      ./common_configuration.nix
  ];

  boot.loader.grub.device  = "/dev/sdc";

  nixpkgs.config.virtualbox.enableExtensionPack = true;

  virtualisation.virtualbox.host.enable = true;

  services = {
    teamviewer.enable = true;
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xrandrHeads = [ "DVI-I-0" "HDMI-0" ];
  };

}
