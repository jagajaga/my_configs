# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, lib, ... }:

{
  require = [
      ./zenbook_hardware-configuration.nix
  ];

  imports = [
      ./common_configuration.nix
  ];

  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName             = "nixosZ";
  };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action) {
      if (action.id == "org.freedesktop.udisks2.filesystem-mount-system") {
          return polkit.Result.YES;
      }
    });
  '';

  services.acpid.enable = true; 
  services.acpid.lidEventCommands = ''
    LID="/proc/acpi/button/lid/LID/state"
        state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
        case "$state" in
            *open*) echo $(whoami) > /home/jaga/whoami ;;
            *close*) 
                # ${pkgs.su}/bin/su jaga -c ${pkgs.slim}/bin/slimlock &
                systemctl suspend 
                ;;
            *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
        esac
        '';
  
  services.upower.enable = true;
  services.teamviewer.enable = false;

  services.xserver = {
    videoDrivers = [ "intel" ];
    /*multitouch.enable   = true;*/
    synaptics = {
      enable = true;
      minSpeed = "0.4";
      maxSpeed = "1.2";
      accelFactor = "0.035";
      palmDetect = true;
      horizontalScroll = false;
      twoFingerScroll = true;
#     buttonsMap = [ 1 3 2 ]; # Some bug, default settings doesn't recognize right touchpad button as it is, now it's two-finger-tap to call menu
      additionalOptions = ''
        Option "TapButton3"           "2"
        Option "ClickPad"             "true"
        Option "SoftButtonAreas"      "50% 0 82% 0 0 0 0 0"
#       Option "PalmMinWidth"         "6"
#       Option "PalmMinZ"             "100"
       '';
     };
  };

}
