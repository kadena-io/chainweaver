{
    hostName,
    location,
    enableHttps,
    nginxPort,
    pactPort,
    pactDataDir,
    pactUser,
    obApp,
    z3,
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
    path = [ z3 ];

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

  services.nginx.virtualHosts."${hostName}" = {
    enableACME = enableHttps;
    forceSSL = enableHttps;
    locations."${location}" = {
      proxyPass = "http://localhost:${toString pactPort}/";
      extraConfig = ''
        # Restrict transaction size:
        client_max_body_size 1m;
        if ($request_method = 'POST') {
           add_header 'Access-Control-Allow-Methods' 'POST';
           add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
           add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range';
        }
        '';
    };

  };
}

