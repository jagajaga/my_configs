pkgs : {
  packageOverrides = pkgs : with pkgs; rec {
      common = import ./common.nix { pkgs = pkgs; }; 
      workEnv = pkgs.buildEnv
      {
        name = "work-env";
        paths = [
          unetbootin
        ];
      };
  };
}
