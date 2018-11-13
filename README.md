# Pact Web

Web IDE for the [Pact](https://pact-language.readthedocs.io/en/latest/) language, including support for deployments to backends (blockchains, test servers).

This is an [Obelisk project](https://github.com/obsidiansystems/obelisk), so find general hacking instructions there.

# Deployments

## Clone the repo

```bash
$ git clone git@github.com:kadena-io/pact-web.git
$ cd pact-web
```
Make sure you have checked out the master branch.

## Deploy

### Initialise `ob deploy` as usual:

```bash
$ ob deploy init <deploydir> --ssh-key <key> --hostname <hostname> --route <URI> --admin-email <email>
```

And change to the just created deployment repo:

```bash
$ cd <deploydir>
```

Where `deploydir` is the same as above.

### Setting up production mode and pact server list

In the `deploydir` directory, create a file at `config/common/pact-server-list`, it can either be empty or it can contain a list of pact servers, in a format like this:

```
    chain01: https://pact01.kadena.io
    chain02: https://pact02.kadena.io
```

You can leave the file empty, in case you prefer to provide the server list at
runtime (see below). Nevertheless it has to exist, otherwise pact-web would run in
`development mode` and the deployment would therefore fail.

### Actual deployment

Use `ob deploy` from the deploy directory:

```bash
$ ob deploy update
$ ob deploy push
```

# Runtime configuration of server-list

In addition to the static configuration from above, the frontend will retrieve a dynamic configuration from the server, if it exists (falling back to the static one).

To provide a dynamic configuration (one that can be changed, without re-deployments) provide a config file at `/var/lib/pact-web/dyn-configs/pact-servers`, format being the same as above. Now, if you reload the page, you should see the server list you specified in `/var/lib/pact-web/dyn-configs/pact-servers`.

Note: If you can do re-deployments: You can also change the static configuration at `config/common/pact-server-list` in your deploy directory and do a re-deploy anytime. Deployments with only the configuration changed are pretty fast.

## Google Analytics tracking

pact-web is set up for basic Google Analytics tracking. You can configure the used tracking id by providing a config file in the deploy directory containing your desired tracking id:

```bash
$ cd <dirname>
$ mkdir config/frontend
$ echo 'your-new-id' > config/frontend/tracking-id
```

Then of course, run the deployment:

```bash
ob deploy push
```

# Deploy pact -s server instances

This repo also includes support for setting up test server backends for
simulating a blockchain. To deploy such a test server do the following:

Change to the pact-server directory:

```bash
$ cd pact-server
```

Run the deploy script:

```bash
$ ./deploy.sh <ssh-key> <hostname> <admin-email>
```

It will deploy a pact -s server to the given hostname using the given ssh-key.
An nginx instance will be setup as a reverse proxy to the pact -s server,
serving its API via https.
