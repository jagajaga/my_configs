
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

  fileSystems."/samba" =
    { device = "//192.168.1.116/Data";
      fsType = "cifs";
      options = "users,nofail";
    };

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/sda1"; }
    ];

}
