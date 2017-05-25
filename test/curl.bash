#!/bin/bash

set -euf -o pipefail

un="user_$$"
pw="pw_$$"

echo "Register"
token=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"$un\", \"password\": \"$pw\"}" \
  http://localhost:8080/register 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/[^=]*=//; s/;.*//'
)

echo "Registration Token: $token"

echo "Login"
token=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"$un\", \"password\": \"$pw\"}" \
  http://localhost:8080/login 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/[^=]*=//; s/;.*//'
)

echo "Login Token: $token"
auth="Authorization: Bearer $token"

echo "Adding Choice"
choiceId=$(
  curl -s --fail -XPOST -H "$auth" -H "Content-Type: application/json" \
  --data '{"choiceName":"Should I get a icecream?"}' \
  http://localhost:8080/choices | jq -r .choiceId
  )

echo "Adding Option"
optionId=$(
  curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":\"$choiceId\", \"optionName\": \"No icecream will make me fat\"}" \
  http://localhost:8080/choices/$choiceId/add | jq -r .optionId
  )

echo "Adding Second Option"
curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":\"$choiceId\", \"optionName\": \"Yes icecream is delicious\"}" \
  http://localhost:8080/choices/$choiceId/add | jq .

echo "Deciding on option $optionId"
curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "\"$optionId\"" \
  http://localhost:8080/choices/$choiceId/choose | jq .

echo "Deciding on option $optionId again!"
failure=$(
  curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "\"$optionId\"" \
  http://localhost:8080/choices/$choiceId/choose || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure did not occur..."
  exit 1
fi

echo "Adding Third Option"
failure=$(
  curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":\"$choiceId\", \"optionName\": \"Yes icecream is delicious\"}" \
  http://localhost:8080/choices/$choiceId/add || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure did not occur..."
  exit 1
fi

echo "Choices"
curl -s --fail -H "$auth" http://localhost:8080/choices | jq .

echo "Me"
curl -s --fail -H "$auth" http://localhost:8080/me | jq .

echo "Choice Info"
curl -s --fail -H "$auth" http://localhost:8080/choices/$choiceId | jq .

echo "Extensions"

echo "Swagger API Spec"
curl -s --fail -H "$auth" http://localhost:8080/swagger.json | jq . > /dev/null && echo Success

echo "Vanilla JS"
curl -s --fail -H "$auth" http://localhost:8080/api-vanilla.js > /dev/null && echo Success

echo "JQuery JS"
curl -s --fail -H "$auth" http://localhost:8080/api-jquery.js > /dev/null && echo Success

echo "Logout"
curl -v --fail -H "$auth" -X POST -H "Content-Type: application/json" \
  http://localhost:8080/logout 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie=deleted; path=/'

echo "Test-Suite Completed"
