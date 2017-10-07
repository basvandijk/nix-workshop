{ config, lib, pkgs, ... } :

with lib;

let
  cfg = config.services.todo-app.db;

  pgPkg = config.services.postgresql.package;

  createDbRoles =
    let PATH = makeSearchPath "bin" [ pkgs.coreutils pkgs.gnugrep pgPkg ];
    in pkgs.writeScript "create-roles-todo-db" ''
      #!/bin/sh
      set -o errexit
      PATH="${PATH}"

      cd ${<todo-app/db>} || exit 1

      ./create_or_update_roles.sh ${cfg.passwordFile}
    '';
in {
  options.services.todo-app.db = {
    enable = mkEnableOption "todo-app DB";

    passwordFile = mkOption {
      type = types.str;
      description = ''
        A file that contains the password of the todo PostgreSQL role.
      '';
    };
  };

  config = mkIf config.services.todo-app.db.enable {
    services.postgresql = {
      enable = true;
      #enableTCPIP = true;
      package = pkgs.postgresql100;
      dataDir = "/var/lib/postgresql/${pgPkg.psqlSchema}";
    };
    systemd.services."create-roles-todo-db" = {
      description = "Create or update roles for the todo_db";
      wants    = [ "postgresql.service" ];
      after    = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart       = createDbRoles;
        Type            = "oneshot";
        RemainAfterExit = "yes";
        User            = "postgres";
      };
    };
  };
}
