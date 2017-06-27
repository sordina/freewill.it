#!/bin/bash

if [ "" != "$JWT_KEY" ]
then
	echo "Using JWT Key from Env variable JWT_KEY"
	echo "$JWT_KEY" > /tmp/key.jwk
fi

/app/vendor/heroku_binaries/latest_heroku_binary --database "$DATABASE_URL" --safeAuth True --jwtKey /tmp/key.jwk --logLevel Prod --port $PORT
