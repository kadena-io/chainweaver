# Pact Web

## Deploy

1. Initialise `ob deploy` as usual:

```bash
$ ob deploy init <dirname> --ssh-key <key> --hostname <hostname> --route <URI> --admin-email <email>
```

2. In the deploy directory, create a file at `config/common/server-url` with
   the full URI and port that the backend will be running at.

3. Update `default.nix` with the relevant details, i.e. hostname, ports, SSL
   certificate paths for CORS.

4. Use `ob deploy` from the deploy directory:

```bash
$ ob deploy update
$ ob deploy push
```
