{
    hostName,
    nginxPort,
    pactPort,
    pactDataDir,
    pactUser,
    obApp,
    pkgs
}: 
let
  pactConfig = pkgs.writeText "pact.yaml" ''
    port: ${toString pactPort}

    logDir: ${pactDataDir}/pact-log
    persistDir: ${pactDataDir}/pact-log

    # SQLite pragmas for pact back-end
    pragmas: []

    # verbose: provide log output
    verbose: True

    gasLimit: 50

    gasRate: 1
  '';
in {pkgs, lib, ...}: {
  users.users.${pactUser} = {
    description = "User for running the pact server (pact -s).";
    isSystemUser = true;
  };

  systemd.services.pact-server = {
    description = "Pact Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      export PATH=$PATH:${pkgs.coreutils}/bin
      mkdir -p ${pactDataDir}/pact-log
      chown ${pactUser} ${pactDataDir}/pact-log
      '';
    serviceConfig = {
      # So preStart runs as root:
      PermissionsStartOnly = true;
      User = pactUser;

      # This is important! pact -s serves files in its working directory!!
      WorkingDirectory = "/var/empty";

      ExecStart = "${pkgs.haskell.lib.justStaticExecutables obApp.ghc.pact}/bin/pact -s ${pactConfig}";
      Restart = "always";
      KillMode = "process";
    };
  };
  networking.firewall.allowedTCPPorts =
    if nginxPort == 443
    then [ 22 80 443 ]
    else [ 22 80 443 nginxPort ];

  services.nginx.appendHttpConfig = ''
      server {
        listen 0.0.0.0:${toString nginxPort} ssl;
        listen [::]:${toString nginxPort} ssl;
        server_name https://${hostName};
        ssl_certificate /var/lib/acme/${hostName}/fullchain.pem;
        ssl_certificate_key /var/lib/acme/${hostName}/key.pem;

        # Restrict transaction size:
        client_max_body_size 1m;

        location / {
          if ($request_method = 'POST') {
             add_header 'Access-Control-Allow-Methods' 'POST';
             add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
             add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range';
          }


          proxy_pass http://127.0.0.1:${toString pactPort};
          proxy_http_version 1.1;
          # proxy_set_header Upgrade $http_upgrade;
          # proxy_set_header Connection "upgrade";
        }
      }
  '';
}

