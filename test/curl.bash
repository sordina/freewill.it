#!/bin/bash

set -euf -o pipefail

echo "Login"
token=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data '{"username":"Ali Baba", "password": "Open Sesame"}' \
  http://localhost:8080/login 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/.*=//; s/;.*//'
)

echo "Token: $token"
auth="Authorization: Bearer $token"

echo "Adding Choice"
choiceId=$(
  curl -s --fail -XPOST -H "$auth" -H "Content-Type: application/json" \
  --data '{"choiceName":"Should I get a icecream?"}' \
  http://localhost:8080/choices | jq -r .choiceId
  )

echo "Adding Option"
optionId=$(
  curl -v --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":$choiceId, \"optionName\": \"No icecream will make me fat\"}" \
  http://localhost:8080/choices/$choiceId/add | jq -r .optionId
  )

echo "Adding Second Option"
curl -v --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":$choiceId, \"optionName\": \"Yes icecream is delicious\"}" \
  http://localhost:8080/choices/$choiceId/add | jq .

echo "Deciding on option $optionId"
curl -v --fail -H "$auth" -H "Content-Type: application/json" \
  --data "$optionId" \
  http://localhost:8080/choices/$choiceId/choose | jq .

echo "Deciding on option $optionId again!"
failure=$(
  curl -v --fail -H "$auth" -H "Content-Type: application/json" \
  --data "$optionId" \
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
  --data "{\"optionChoiceId\":$choiceId, \"optionName\": \"Yes icecream is delicious\"}" \
  http://localhost:8080/choices/$choiceId/add || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure did not occur..."
  exit 1
fi

echo "Choices"
curl -s --fail -H "$auth" http://localhost:8080/choices | jq .

echo "Choice Info"
curl -s --fail -H "$auth" http://localhost:8080/choices/$choiceId | jq .

echo "Extensions"

echo "Swagger API Spec"
curl -s --fail -H "$auth" http://localhost:8080/swagger.json | jq .

echo "Vanilla JS"
curl -s --fail -H "$auth" http://localhost:8080/vanilla.js
