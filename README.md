# Pact Web

## Clone the repo

```bash
$ git clone git@github.com:kadena-io/pact-web.git
$ cd pact-web
```

Make sure you have checked out the master branch.

## Deploy

1. Initialise `ob deploy` as usual:

```bash
$ ob deploy init <dirname> --ssh-key <key> --hostname <hostname> --route <URI> --admin-email <email>
```

2. Use `ob deploy` from the deploy directory (<dirname>):

```bash
$ cd <dirname>
$ ob deploy update
$ ob deploy push
```

## Troubleshooting

If after successful deployment the pact backend does not seem to be accessible, this might be caused by some firewall sitting in front of your server. If you have such a firewall (Amazon seems to have one), make sure to enable the port you can find in <dirname>/config/common/pact-port . You can also change the port in this file, if need be.

Also because we are using a non standard port (7010) by default, the pact backend might not be accessible for users having themself some kind of strict firewall. This can be fixed by using a dedicated server for the pact backend, which will become easy with the next release.
