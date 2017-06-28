#!/bin/bash

set -e

docker-build-and-run

if [ ! -f .dockername ]
then
	echo "Couldn't find .dockername" 1>&2
	exit 1
fi

containername=$(
	cat .dockername
)

containerid=$(
	docker ps -a | grep $dockername | sed 's/ .*//'
)

artefactname="~/freewill.docker-`date-human`-`git rev-parse --short HEAD`.bin"

docker cp $containerid:/freewill/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/freewill/freewill ./temp/$artefactname
aws --profile=sordina s3 cp ./temp/$artefactname s3://sordina.binaries/$artefactname --acl public-read
echo "s3://sordina.binaries/$artefactname" > heroku_binary_location
