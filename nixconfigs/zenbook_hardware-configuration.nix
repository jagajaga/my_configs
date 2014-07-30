
# This is a generated file.  Do not modify!
# Make changes to /etc/nixos/configuration.nix instead.
{ config, pkgs, ... }:

{
  require = [
    <nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.kernelModules = [ "ehci_hcd" "ahci" "xhci_hcd" "usb_storage" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  nix.maxJobs = 4;

  fileSystems."/".device = "/dev/sda2";
  fileSystems."/home".device = "/dev/sda3";

  /*fileSystems."/media/Arch" =     # where you want to mount the device*/
    /*{ device = "/dev/sda4";  # the device*/
      /*fsType = "ext4";      # the type of the partition*/
      /*options = "data=journal,users,rw,user,auto,exec";*/
    /*};*/

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/sda1"; }
    ];

}
