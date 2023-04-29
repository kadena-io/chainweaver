# Chainweaver 

Kadena Chainweaver desktop wallet and web-based playground for the [Pact](https://pact-language.readthedocs.io/en/latest/) language, including support for deployments to backends (blockchains, test servers).

# Hacking

This is an [Obelisk project](https://github.com/obsidiansystems/obelisk), so find general hacking instructions there.

Since release 1.4 chainweaver has support for GitHub gist sharing so it needs client credentials for operation. See section `Deployments` for details on how to obtain them and put the client id and the client secret in the appropriate config files.

The homepage URL will be http://localhost:8000, the authorization callback url will be http://localhost:8000/oauth/redirect/github .

Image files are located in [static/img](./static/img). To test an updated image, replace the old file with the new one and refresh the browser.

## Linux Based Chainweaver
A linux webkitgtk version of chainweaver can be built and run in a number of different ways:
- Run `nix-build -A deb default.nix` to build a debian package of chainweaver.
  - This can be installed with `dpkg -i <deb file>` on ubuntu 18.04
  - Chainweaver should be accessible in the applications menu.
- Run `nix-build -A chainweaverVM default.nix` to build a virtualbox ova file that runs chainweaver
- Run `WEBKIT_DISABLE_COMPOSITING_MODE=1 $(nix-build -A nixosExe)/bin/kadena-chainweaver` to run the linux app in nixos.

NOTE: You must build the OVA on NixOS with the `kvm` system-feature enabled. You
can set this by putting the following in `$HOME/.config/nix/nix.conf`.

```
system-features = kvm benchmark big-parallel nixos-test
```

### OVA Release

The OVA includes an upgrade script inside the VM that allows the user to upgrade the nixos configuration without redownloading the whole OVA.

This is powered by the http://nixcache.kadena.io binary cache and a file in s3 that contains the latest nixos system store path. See https://chainweaver-builds.s3.amazonaws.com/vm/master-store-path

To release a new version, be sure that CI is pushing to that binary cache, and when the build passes QA overwrite the contents of master-store-path with the output of `nix-build -A chainweaverVMSystem default.nix` by running `scripts/update-vm-master-store`. This requires permission to write to the chainweaver-builds bucket and to have credentials in ~/.aws/credentials already.

At this point, users ought to be able to run `upgrade-chainweaver` from a terminal to switch to the new system config.

## Mac app
To work on the objective-c code enter the project shell with
```shell
nix-shell default.nix -A shells.ghc
```
then (re-)enter `cabal new-repl mac` everytime you change the objective-c code.

To actually run:
```shell
nix-build -A mac
open result/*.app
```

## Apple Disk Image

### Mac Developer Certificate
Create and download a "Mac Developer" certificate in https://developer.apple.com/account/resources/certificates/list - opening the downloaded file should launch *Keychain Access* and display your new certificate.

### Apple Worldwide Developer Relations (WWDR) Certificate
Find it in https://www.apple.com/certificateauthority/ under "Apple Intermediate Certificates". Download and open to add it to *Keychain Access*.

### Deploying

<!-- TODO: should we also accept "Apple Development" certificates? -->
<!-- TODO: the deploy script will give a helpful error message if no "Mac Developer" certificate is found for the given TEAM_ID. We should do the same for the other pre-requisites -->

```shell
nix-build -A deployMac
./result TEAM_ID
```
where `TEAM_ID` can be found in the upper-right corner after the team name in https://developer.apple.com/account/resources/ .

Wait for the script to end. Do not interact with the installer window that pops up as that might prevent the disk image from being unmounted.
A .dmg should have been created in the current directory. To test it's working properly:
- launch the installer window by opening the .dmg file
- right click in the app icon and select "Open" (do not simply double-click at it does not allow you to proceed)
- a dialog saying "the identity of the developer cannot be confirmed" should appear - click "Open"

OS X specific assets are located in the [static](./mac/static) folder. To test an updated asset, replace the old file and build again. Use 1024x1024 *.png* files for Mac app [icons](./mac/static/icons).

# Deployments

## Clone the repo

```bash
$ git clone git@github.com:kadena-io/chainweaver.git
$ cd chainweaver
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

### Setting up production mode and the network list

In the `deploydir` directory, create a file at `config/common/networks`, it must contain a list of networks. With each network consisting of at least one host. A network can either be some chainweb deployment or also some pact -s server. chainweaver will auto detect what it actually is:

```
    testnet: api.testnet.chainweb.com
    mainnet: api.chainweb.com
```

This file must exist, so chainweaver won't start up in development mode (launching its own pact servers), as of this writing it should also be populated with entries.

The first entry in the file will be chosen as the current network, unless the user picked a different one. The user will also be able to modify networks at runtime. The above configuration will be the default and the one that gets applied, when the user presses "Restore Defaults".

### Remote verification server

chainweaver supports verification of Pact modules, unfortunately the prover used is z3 which is implemented in C++ and is therefore not available on ghcjs. To make it still work, we use a remote verification server for verifying contracts.

In order to deploy the remote verification server along with the chainweaver deployment copy the file `pact-server/module.nix` to the `deploydir`.
Based on the deployment target please edit the imports in this file to use either `mkBaceEc2` or `virtualbox-image.nix`.

The other optional configurable parameter in this file is the `location`, which is the nginx virtualhost's path.
Its default value is `/pact/`, but it could be modified to some other value.

Finally make sure the `config/common/verification-server` file matches the location value specified in the `module.nix`.

For the default location of `/pact/`, the contents of this file would be `<URI>/pact`, for example:
```
https://my-chainweaver.io/pact
```

In case the pact server is not deployed with the chainweaver, please provide a file `config/common/verification-server` containing the base url of some other `pact -s` server, e.g.:

```
https://pact01.kadena.io

```

### OAuth GitHub configuration

chainweaver needs GitHub client credentials for the Gist sharing feature. So before deployment, you need to provide those credentials via obelisk executable configs.

To create a GitHub OAuth application, login into GitHub, then go to `Settings` of either your GitHub account or of an organization that should host the application. There you can find `Developer Settings` allowing you to create an OAuth application.

Direct links:

- Create a personal oAuth application, belonging to your account [here](https://github.com/settings/developers)
- Create an OAuth application belonging to an organization here: https://github.com/organizations/YOUR-ORGANIZATION/settings/applications

Replace `YOUR-ORGANIZATION` with your organization's name.

Press `Register an application` or `New application`, then give it a name of your liking and fill out the url, which should match the url where chainweaver will be running. For the Authorization callback URL use the following:

https://HOST-WHERE-CHAINWEAVER-RUNS/contracts/oauth/redirect/github


Replace `HOST-WHERE-CHAINWEAVER-RUNS` with your actual host name and press `Register application`.

You will be presented with a screen showing your application's client id and its client secret.

The client id must go into `config/common/oauth/github/client-id`. The client secret must go into `config/backend/oauth/github/client-secret` of your deployment directory (or in your checked out source repository, when setting up a developer environment).

## Google Analytics tracking

chainweaver is set up for basic Google Analytics tracking. You can configure the used tracking id by providing a config file in the deploy directory containing your desired tracking id:

```bash
$ cd <dirname>
$ mkdir config/frontend
$ echo 'your-new-id' > config/frontend/tracking-id
```

Then of course, run the deployment:

```bash
ob deploy push
```

### Actual deployment

Use `ob deploy` from the deploy directory:

```bash
$ ob deploy update
$ ob deploy push
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

# Installation & Updating Instructions

## Ubuntu Package

To install the ubuntu package, download the .deb, open up a terminal and run:

`dpkg -i Downloads/kadena-chainweaver_YYYY.MM.DD_amd64.deb`

Follow the same process for updating chainweaver. The package manager will detect the old version and do the appropriate upgrades if chainweaver is already installed.

The .deb has been tested on Ubuntu 18.04. It should work on other debian based linuxes and Ubuntus, but Ubuntu 18.04 is the only supported disribution at the moment.

Chainweaver does not support wayland. To run it on Ubuntu 20.04 or 22.04, override `GDK_BACKEND`:

```
GDK_BACKEND=x11 kadena-chainweaver
```

## OVA (if the Ubuntu Package or Mac App doesn't work for you)

First, you will need Virtualbox: https://www.virtualbox.org/wiki/Downloads . If you are using this on a linux machine, it is highly recommended to use virtualbox from your system's package manager as the virtualbox installer often leaves off important components.

To install the virtualbox appliance, download the OVA and double click on it in your downloads folder. This should open up an import dialog in Virtualbox. You can safely accept the default options.

Once imported, start the virtual machine by double clicking on it in the list of VMs in virtualbox. This will start a linux VM that will run chainweaver on login. You can safely delete the OVA file now, as the appliance is imported and can be run just be starting Virtualbox and double clicking the chainweaver VM.

If double clicking on the OVA didn't work, try opening VirtualBox and clicking File > Import Appliance and selecting the OVA from the file chooser. Clicking through and accepting the defaults should be fine.

### Upgrading

Because the OVA file is quite large, there exists a method to update chainweaver inside the virtual machine without redownloading the entire appliance. Press on "Applications" in the bottom right of the screen, then click "Other" and then "Upgrade Kadena Chainweaver". This should update your chainweaver in place. You will want to restart chainweaver at this point.

### Snapshotting

The nice aspect of using a virtual machine is taking advantage of virtualbox's ability to snapshot the disk of the virtual machine. You can back up your chainweaver wallet prior to upgrading if you want an absolute peace of mind.

To take a snapshot (if the machine is running), Click on the "Machine" menu of Virtualbox and click "Take Snapshot".

To restore from a snapshot, go to the VirtualBox manager, click "Snapshot" and click "Restore".
