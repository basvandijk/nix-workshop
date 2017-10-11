# For testing the nixtodo network each machine can be deployed to a
# container running on a NixOS host. This host is typically your own
# workstation.
#
# After deploying the containers the `nixos-container` command can be
# used to interact with the machines.
{
  backend = { lib, ... }: {
    deployment.targetEnv = "container";

    # Setting up a working TLS implementation (HTTPS) in a testing
    # environment can be difficult because you have to install
    # self-signed certificates and configure your browsers and other
    # clients to use it.
    #
    # Because of this we disable TLS in our containerized
    # network. Note how we use the `mkForce` function from `lib` to
    # override the options which were already defined elsewhere.
    services.nginx = with lib; {
      recommendedTlsSettings = mkForce false;
      virtualHosts."nixtodo.com" = {
        forceSSL   = mkForce false;
        enableACME = mkForce false;
      };
    };
  };
}
