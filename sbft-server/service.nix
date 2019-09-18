{
    hostName,
    myDbFile,
    myApiPort,
    myAlias,
    myPort,
    myPublicKey,
    mySignerSecret,
    mySignerPublic,
    myEphemeralSecret,
    myEphemeralPublic,
    myStaticSecret,
    myStaticPublic,
    myAdminKey,
    myPrivateKey,
    alias-a,
    port-a,
    publicKey-a,
    remote-static-a,
    remote-name-a,
    alias-b,
    port-b,
    publicKey-b,
    remote-static-b,
    remote-name-b,
    alias-c,
    port-c,
    publicKey-c,
    remote-static-c,
    remote-name-c,
    sbftUser,
    pkgs
}:
let
  pactConfig = pkgs.writeText "sbft.yaml" ''
    pactPersist:
      writeBehind: false
      backend:
        config:
          _pragmas:
          - synchronous = OFF
          - journal_mode = MEMORY
          - locking_mode = EXCLUSIVE
          - temp_store = MEMORY
          _dbFile: ${toString myDbFile}
        type: SQLITE
    enableDiagnostics: null
    publicKeys:
      node0: ${toString myPublicKey}
      node1: ${toString publicKey-a}
      node2: ${toString publicKey-b}
      node3: ${toString publicKey-c}
    aeBatchSize: 20000
    heartbeatTimeout: 2000000
    logDir: ./log
    apiPort: ${toString myApiPort}
    logRules:
      PactService:
        include: null
        enable: null
        exclude:
        - DEBUG
    clusterMembers:
    changeToNodes: []
    otherNodes:
    - alias: ${toString alias-a}
      fullAddr: tcp://127.0.0.1:${toString port-a}
      host: 127.0.0.1
      port: ${toString port-a}
    - alias: ${toString alias-b}
      fullAddr: tcp://127.0.0.1:${toString port-b}
      host: 127.0.0.1
      port: ${toString port-b}
    - alias: ${toString alias-c}
      fullAddr: tcp://127.0.0.1:${toString port-c}
      host: 127.0.0.1
      port: ${toString port-c}
    preProcThreadCount: 10
    hostStaticDir: true
    electionTimeoutRange:
    - 10000000
    - 18000000
    inMemTxCache: 200000
    myPublicKey: ${toString myPublicKey}
    nodeId:
      alias: ${toString myAlias}
      fullAddr: tcp://127.0.0.1:${toString myPort}
      host: 127.0.0.1
      port: ${toString myPort}
    enableDebug: true
    entity:
      signer:
        secret: ${toString mySignerSecret}
        public: ${toString mySignerPublic}
      local:
        ephemeral:
          secret: ${toString myEphemeralSecret}
          public: ${toString myEphemeralPublic}
        static:
          secret: ${toString myStaticSecret}
          public: ${toString myStaticPublic}
        name: Alice
      sending: true
      remotes:
      - static: ${toString remote-static-a}
        name: ${toString remote-name-a}
      - static: ${toString remote-static-b}
        name: ${toString remote-name-b}
      - static: ${toString remote-static-c}
        name: ${toString remote-name-c}
    adminKeys:
      admin0: ${toString myAdminKey}
    nodeClass: active
    enablePersistence: true
    myPrivateKey: ${toString myPrivateKey}
    preProcUsePar: true
'';

in {pkgs, lib, ...}: {
  users.users.${sbftUser} = {
    description = "User for running the sbft server instance.";
    isSystemUser = true;
  };

  systemd.services.pact-server = {
    description = "SFBT Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      export PATH=$PATH:${pkgs.coreutils}/bin
      '';
    serviceConfig = {
      # So preStart runs as root:
      PermissionsStartOnly = true;
      User = sbftUser;

      # This is important! pact -s serves files in its working directory!!
      #WorkingDirectory = "/var/empty";

      #ExecStart = "${pkgs.haskell.lib.justStaticExecutables obApp.ghc.pact}/bin/pact -s ${pactConfig}";
      # module Apps.Kadena.Server
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
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString myApiPort}";
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
