# Pact Web

## Clone the repo

```bash
git clone git@github.com:kadena-io/pact-web.git
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
