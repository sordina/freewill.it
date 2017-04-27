#!/bin/bash

set -e

echo 'drop   database freewill;' | psql
echo 'create database freewill;' | psql
migrate.sh freewill
