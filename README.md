# Pact Web

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
    working-agreement: https://working-agreement.obsidian.systems:7010
    other-server-name: https://some.other.pact.backend.server
```

You can leave the file empty, in case you prefer to provide the server list at
runtime (see below). Nevertheless it has to exist, otherwise pact-web will run in
`development mode`.

In the default deployment one pact instance is running at port 7010, so to get a working configuration out of the box, use a configuration like the following:

```
    <hostname>: https://<hostname>:7010
```

Replace `hostname` with your actual hostname, like in the `deploy init` command above.

Note however, that there will be users having pretty restrictive firewalls preventing their browser to access non-standard ports like 7010, therefore we recommend to spawn pact server instances at well-known ports like `443`.

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

```
$ cd <dirname>
$ mkdir config/frontend
$ echo 'your-new-id' > config/frontend/tracking-id
```

Then of course, run the deployment:

```
ob deploy push
```
