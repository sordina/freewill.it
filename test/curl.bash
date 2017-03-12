#!/bin/bash

set -e

echo "Adding Choice"
choiceId=$(
  curl -s -XPOST -H "Content-Type: application/json" \
  --data '{"choiceName":"Should I get a icecream?"}' \
  http://localhost:8080/choices | jq -r .choiceId
  )

echo "Adding Option"
optionId=$(
  curl -s -XPOST -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":$choiceId, \"optionName\": \"No icecream will make me fat\"}" \
  http://localhost:8080/choices/$choiceId/add | jq -r .optionId
  )

echo "Adding Second Option"
curl -s -XPOST -H "Content-Type: application/json" \
  --data "{\"optionChoiceId\":$choiceId, \"optionName\": \"Yes icecream is delicious\"}" \
  http://localhost:8080/choices/$choiceId/add | jq .

echo "Deciding on option $optionId"
curl -v -XPOST -H "Content-Type: application/json" \
  --data "$optionId" \
  http://localhost:8080/choices/$choiceId/choose | jq .

echo "Choices"
curl -s http://localhost:8080/choices | jq .

echo "Choice Info"
curl -s http://localhost:8080/choices/$choiceId | jq .
