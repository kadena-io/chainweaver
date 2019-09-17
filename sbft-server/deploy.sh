#!/usr/bin/env bash
set -e
if [[ $# != 4 ]]
then
  echo "Usage: ./deploy.sh keyfile hostName default-nix-file adminEmail"
  exit -1
fi
key=$1
hostName=$2
defaultNixFile=$3
adminEmail=$4

export NIX_SSHOPTS="-i${key}"

target=$(nix-build -E "(import \"${defaultNixFile}\" { hostName = \"${hostName}\"; adminEmail = \"${adminEmail}\"; }).system")

echo ${target}

echo "Copying closure ..."
nix-copy-closure -v --to "root@${hostName}" --gzip ${target}

echo "Activating configuration ..."

ssh -i ${key} root@${hostName} nix-env -p /nix/var/nix/profiles/system --set ${target}

ssh -i ${key} root@${hostName} /nix/var/nix/profiles/system/bin/switch-to-configuration switch
