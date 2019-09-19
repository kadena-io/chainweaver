{
    hostName,
    dbFile,
    apiPort,
    alias,
    fullAddr,
    host,
    port,
    publicKey,
    signerSecret,
    signerPublic,
    ephemeralSecret,
    ephemeralPublic,
    staticSecret,
    staticPublic,
    localName,
    adminKey,
    privateKey,
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
          _dbFile: ${toString dbFile}
        type: SQLITE
    enableDiagnostics: null
    publicKeys:
      node0: ${toString publicKey}
      node1: ${toString publicKey-a}
      node2: ${toString publicKey-b}
      node3: ${toString publicKey-c}
    aeBatchSize: 20000
    heartbeatTimeout: 2000000
    logDir: ./log
    apiPort: ${toString apiPort}
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
    myPublicKey: ${toString publicKey}
    nodeId:
      alias: ${toString alias}
      fullAddr: ${toString fullAddr}
      host: 127.0.0.1
      port: ${toString port}
    enableDebug: true
    entity:
      signer:
        secret: ${toString signerSecret}
        public: ${toString signerPublic}
      local:
        ephemeral:
          secret: ${toString ephemeralSecret}
          public: ${toString ephemeralPublic}
        static:
          secret: ${toString staticSecret}
          public: ${toString staticPublic}
        name: ${toString localName}
      sending: true
      remotes:
      - static: ${toString remote-static-a}
        name: ${toString remote-name-a}
      - static: ${toString remote-static-b}
        name: ${toString remote-name-b}
      - static: ${toString remote-static-c}
        name: ${toString remote-name-c}
    adminKeys:
      admin0: ${toString adminKey}
    nodeClass: active
    enablePersistence: true
    myPrivateKey: ${toString privateKey}
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
      Restart = "always";
      KillMode = "process";
    };
  };
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];

  services.nginx.virtualHosts."${hostName}" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString apiPort}";
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
