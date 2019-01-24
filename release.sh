#!/usr/bin/env bash

name=$1

if [[ -z $1 ]]
then
    echo "Usage: ./release.sh new-version"
fi

for i in */*.cabal
do
    sed -i "s/^version:.*/version:             ${name}/1" ${i}
    git add ${i}
done

git commit -m "Version bump"
git tag -s v${name}

