#!/bin/bash

set -euf -o pipefail

API=${1-"http://localhost:8080"}

echo "Invalid Username Registration"
failure=$(
  curl -s --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"noemail\", \"password\": \"password\"}" \
  $API/register || echo failed
)

if [[ "$failure" != failed ]]
then
  echo "Expected failure did not occur..."
  exit 1
fi

echo "Failed as expected"

echo "User Registration"

un="user_$$@email.com"
pw="pw_$$"

echo "User: $un"
echo "Password: $pw"

token=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"$un\", \"password\": \"$pw\"}" \
  $API/register 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/[^=]*=//; s/;.*//'
)

echo "Registration Token: $token"

echo "Login"
token=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"$un\", \"password\": \"$pw\"}" \
  $API/login 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/[^=]*=//; s/;.*//'
)

echo "Login Token: $token"
auth="Authorization: Bearer $token"

echo "Adding Choice"
choiceId=$(
  curl -s --fail -XPOST -H "$auth" -H "Content-Type: application/json" \
  --data '{"choiceName":"Should I get a icecream?"}' \
  $API/choices | jq -r .choiceId
  )

echo "Adding Option"
optionId=$(
  curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":\"$choiceId\", \"optionName\": \"No icecream will make me fat\"}" \
  $API/choices/$choiceId/add | jq -r .optionId
  )

echo "Adding Second Option"
curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":\"$choiceId\", \"optionName\": \"Yes icecream is delicious\"}" \
  $API/choices/$choiceId/add | jq .

echo "Deciding on option $optionId"
curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "\"$optionId\"" \
  $API/choices/$choiceId/choose | jq .

echo "Deciding on option $optionId again!"
failure=$(
  curl -s --fail -H "$auth" -H "Content-Type: application/json" \
  --data "\"$optionId\"" \
  $API/choices/$choiceId/choose || echo failed
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
  $API/choices/$choiceId/add || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure did not occur..."
  exit 1
fi

echo "Testing Sharing"
un2="user_2_$$@email.com"
pw2="pw_2_$$"

echo "User: $un2"
echo "Password: $pw2"

token2=$(
  curl -v --fail -XPOST -H "Content-Type: application/json" \
  --data "{\"username\":\"$un2\", \"password\": \"$pw2\"}" \
  $API/register 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie' | sed 's/[^=]*=//; s/;.*//'
)

echo "Login Token: $token"
auth2="Authorization: Bearer $token2"

echo "Attempting to view unshared choice"
failure=$(
  curl -s --fail -H "$auth2" $API/choices/$choiceId || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure to view unshared choice did not occur..."
  exit 1
fi

echo "Sharing Choice"
curl -s --fail -H "$auth" -X POST -H "Content-Type: application/json" \
  $API/choices/$choiceId/share | jq .

echo "Choice should now be visible to other users"
curl -s --fail -H "$auth2" $API/choices/$choiceId | jq .

echo "Hiding Choice"
curl -s --fail -H "$auth" -X POST -H "Content-Type: application/json" \
  $API/choices/$choiceId/hide | jq .

echo "Attempting to view re-hidden choice"
failure=$(
  curl -s --fail -H "$auth2" $API/choices/$choiceId || echo failed
  )

if [[ "$failure" != failed ]]
then
  echo "Expected failure to view re-hidden choice did not occur..."
  exit 1
fi

echo "Choices"
curl -s --fail -H "$auth" $API/choices | jq .

echo "Me"
curl -s --fail -H "$auth" $API/me | jq .

echo "Choice Info"
curl -s --fail -H "$auth" $API/choices/$choiceId | jq .

echo "Extensions"

echo "Swagger API Spec"
curl -s --fail -H "$auth" $API/swagger.json | jq . > /dev/null && echo Success

echo "Vanilla JS"
curl -s --fail -H "$auth" $API/api-vanilla.js > /dev/null && echo Success

echo "JQuery JS"
curl -s --fail -H "$auth" $API/api-jquery.js > /dev/null && echo Success

echo "Logout"
curl -v --fail -H "$auth" -X POST -H "Content-Type: application/json" \
  $API/logout 2>&1 \
  | grep 'Set-Cookie: JWT-Cookie=deleted; path=/'

echo "Test-Suite Completed"
