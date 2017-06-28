#!/bin/bash

set -e

if [ "" != "$JWT_KEY" ]
then
	echo Using JWT_KEY from env
	/app/vendor/heroku_binaries/latest_heroku_binary --database "$DATABASE_URL" --safeAuth True --jwtKey <(echo "$JWT_KEY") --logLevel Prod --port $PORT --minified
else
	echo Using random JWT_KEY
	/app/vendor/heroku_binaries/latest_heroku_binary --database "$DATABASE_URL" --safeAuth True --logLevel Prod --port $PORT --minified
fi
