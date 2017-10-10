{
  backend = { lib, ... }: with lib; {
    deployment = {
      targetEnv = "container";
    };
    services.nginx = {
      recommendedTlsSettings = mkForce false;
      virtualHosts."nixtodo.com" = {
        forceSSL   = mkForce false;
        enableACME = mkForce false;
      };
    };
  };
}
