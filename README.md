# Pact Web

## Deploy

1. Initialise `ob deploy` as usual:

```bash
$ ob deploy init <dirname> --ssh-key <key> --hostname <hostname> --route <URI> --admin-email <email>
```

1. In the deploy directory, create a file at `config/common/server-url` with
   the full URI and port that the pact backend will be running at.

   E.g.: `https://working-agreement.obsidian.systems:7011`

2. Update `default.nix` with the relevant details:
  1. hostname should match the one in config/common/server-url, in our example this would be: `working-agreement.obsidian.systems`
  2. nginxPort should match the port in `config/common/server-url`, in our example this would be `7011`.
  3. all other values should be fine.

3. Use `ob deploy` from the deploy directory:

```bash
$ ob deploy update
$ ob deploy push
```
