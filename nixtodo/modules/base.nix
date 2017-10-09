{ lib, pkgs, ... }:

with lib;

let engineers = with builtins;
      genAttrs (attrNames (readDir ./users)) (name :
        let userDir = ./users + "/${name}"; in {
          inherit name;
          isNormalUser = true;
          extraGroups = [ "systemd-journal" "nixtodo" ];
          openssh.authorizedKeys.keys =
            let sshPublicKeyPath = userDir + /.ssh/id_rsa.pub;
            in optional (pathExists sshPublicKeyPath)
                        (readFile sshPublicKeyPath);
        }
      );
in {
  imports = [ <nixtodo/modules> ];

  nixpkgs.overlays = [ (import <nixtodo/nix/overlay.nix>) ];

  environment.systemPackages = with pkgs; [ htop ];

  services.openssh.enable = true;

  networking.firewall.allowPing = true;

  # security = {
  #   sudo.wheelNeedsPassword = false;
  #   initialRootPassword = "!";
  # };

  # users = {
  #   mutableUsers = false;

  #   groups.nixtodo = { name = "nixtodo"; };

  #   users = engineers // {
  #     "root" = {
  #        openssh.authorizedKeys.keys =
  #          concatMap (user : user.openssh.authorizedKeys.keys)
  #                    (attrValues engineers);
  #     };
  #   };
  # };
}
