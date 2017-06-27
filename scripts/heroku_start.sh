#!/bin/bash

if [ "" != "$JWT_KEY" ]
then
	echo "Using JWT Key from Env variable JWT_KEY"
	echo "$JWT_KEY" > /app/key.jwk
fi

/app/vendor/heroku_binaries/latest_heroku_binary --database "$DATABASE_URL" --safeAuth True --jwtKey /app/key.jwk --logLevel Prod --port $PORT
