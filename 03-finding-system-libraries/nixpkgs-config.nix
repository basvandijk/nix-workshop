# ~/.nixpkgs/config.nix

{ pkgs }: {

  packageOverrides = super: let self = super.pkgs; in {

    system-libraries-env = self.buildEnv {
      name = "system-libraries-0";
      paths = [self.openssl_1_1_0];
      extraOutputsToInstall = ["out" "dev"];
    };

  };

}
