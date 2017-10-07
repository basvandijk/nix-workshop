{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.todo-app.server;

  todoBackendConf = pkgs.writeTextFile {
    name = "todo-backend.conf";
    text =  ''
      db
      {
        host         = "127.0.0.1"
        port         = "${toString config.services.postgresql.port}"
        database     = "todo_db"
        user         = "todo"
        passwordFile = "${cfg.db.passwordFile}"

        pool
        {
          numStripes   = 2
          maxResources = 25
          idleTime     = 60
        }
      }

      frontendIndexTemplater
      {
        indexTemplatePath = "${pkgs.todo.frontend-static}/index.html.mustache"
        srcDir            = "${pkgs.todo.frontend-static}/static"
        dstDir            = "hashed-frontend"
        urlPrefix         = "hashed"
        compressLevel     = 9
      }

      web-server
      {
        host = "127.0.0.1"
        port = "${toString cfg.web-server.port}"
      }

    '';
  };

  user  = "todo-backend";
  group = user;

  stateDir = "/var/lib/todo-backend";

  migrateDb =
    let PATH = makeSearchPath "bin" [ pkgs.coreutils pkgs.gnugrep config.services.postgresql.package ];
        db = <todo-app/db>;
        migrate = ''
          port="${toString config.services.postgresql.port}"
          todoPasswordFile="${cfg.db.passwordFile}"
          ${db + "/create_or_update_db.sh"} "todo_db" "$port" "$todoPasswordFile" "migrations"

          export PGPASSWORD
          PGPASSWORD="$(cat $todoPasswordFile)"
        '';
    in pkgs.writeScript "migrate-todo-db" ''
      #!/bin/sh
      set -o errexit
      PATH="${PATH}"

      if [ "${stateDir}/latest-migration" -ef "${db}" ]; then
        echo "No need to migrate the todo-db because it has already been migrated with ${db}"
        exit
      fi

      cd ${db} || exit 1

      ${migrate}
      ln -sfT "${db}" "${stateDir}/latest-migration"
    '';

in {
  options.services.todo-app.server = {
    enable = mkEnableOption "todo-app backend server";

    db.passwordFile = mkOption {
      type = types.str;
      description = ''
        A file that contains the password of the todo-app PostgreSQL role.
      '';
    };

    web-server.port = mkOption {
      type = types.int;
      default = 8000;
      description = ''
        Port to listen on (for http).
      '';
    };

  };

  config = mkIf cfg.enable {
    systemd.services.todo-backend = {
      description = "todo-backend";
      wants = [ "create-roles-todo-db.service" ];
      after = [ "create-roles-todo-db.service" "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = user;
        Group = group;
        WorkingDirectory = stateDir;
        ExecStartPre = migrateDb;
        ExecStart = "${pkgs.haskellPackages.todo-backend}/bin/todo-backend --config=${todoBackendConf}";
        Restart = "always";
      };
    };

    users = {
      users."${user}" = {
        name  = user;
        group = group;
        home  = stateDir;
        createHome = true;
      };
      groups."${group}" = {
        name = group;
      };
    };
  };
}
