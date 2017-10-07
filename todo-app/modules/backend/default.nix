{ config, lib, pkgs, ... }:

with lib;

let backendPort = config.services.todo-app.server.web-server.port;
in {
  options.services.todo-app.enable = mkEnableOption "todo-app";

  config = mkIf config.services.todo-app.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    services = {
      nginx = {
        enable = true;
        recommendedTlsSettings   = true;
        recommendedOptimisation  = true;
        recommendedProxySettings = true;
        sslDhparam = <todo-app/secrets/dhparams.pem>;
        upstreams = {
          "todo-app-backend" = {
            servers = { "127.0.0.1:${toString backendPort}" = {}; };
          };
        };
        virtualHosts = {
          "nixtodo.com" = {
            default    = true;
            forceSSL   = true;
            enableACME = true;
            locations = {
              "/" = {
                proxyPass = "http://todo-app-backend";
                proxyWebsockets = true;
              };
            };
          };
        };
      };

      todo-app.server = {
        enable = true;
        db.passwordFile = config.services.todo-app.db.passwordFile;
      };

      todo-app.db = {
        enable = true;

        # TODO: don't store the password in the world readable Nix store!
        passwordFile = toString (pkgs.writeTextFile {
          name = "postgresql-todo-role-password";
          text = fileContents <todo-app/secrets/postgresql-todo-role-password>;
        });
      };
    };
  };
}
