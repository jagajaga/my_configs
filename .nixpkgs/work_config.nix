pkgs : {
  allowUnfree = true;
  /*allowBroken = true;*/
  packageOverrides = pkgs : with pkgs; rec {
      common = import ./common.nix { pkgs = pkgs; }; 
      inherit (common) develEnv steamEnv;
      workEnv = pkgs.buildEnv
      {
        name = "work-env";
        paths = [
          pavucontrol
          unetbootin
        ];
      };
  };
}
