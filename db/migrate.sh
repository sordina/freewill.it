#!/bin/sh

set -e

# Options with defaults

SCRIPT_ARGS="$*"
MIGRATION_DIR=${MIGRATION_DIR-'migrations'}
MIGRATION_TABLE=${MIGRATION_TABLE-'migrations'}
MIGRATION_COMMAND=${MIGRATION_COMMAND-'psql'}
DATABASE_OPTIONS=${DATABASE_OPTIONS-''}

help () {
	echo
	echo "  Usage: migrate.sh"
	echo
	echo "  Options:"
	echo "    * MIGRATION_DIR      Default: 'migrations'"
	echo "    * MIGRATION_TABLE    Default: 'migrations'"
	echo "    * MIGRATION_COMMAND  Default: 'psql'"
	echo "    * DATABASE_OPTIONS"
	echo
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]
then
	help
	exit 0
fi

require () {
	command -v "$1" >/dev/null 2>&1 || (echo "command $1 is required" && exit 1)
}

require date
require cat
require grep
require echo
require find
require "$MIGRATION_COMMAND"

if [ -z "$DATABASE_OPTIONS$SCRIPT_ARGS" ]
then
	echo
	echo "Please ensure that you specify a database either as an argument or in \$DATABASE_OPTIONS."
	help
	exit 1
fi

DOSQL="$MIGRATION_COMMAND $DATABASE_OPTIONS $SCRIPT_ARGS"

test_setup () {
	echo "testing for $MIGRATION_TABLE table"
	echo "select count(*) from $MIGRATION_TABLE;" | $DOSQL | grep -v ERROR > /dev/null
}

setup () {
	echo "setting up $MIGRATION_TABLE table"
	echo "create table $MIGRATION_TABLE(migration_id text);" | $DOSQL
}

test_ran () {
	BASE=$(basename "$1")
	echo "testing for migration $1"
	echo "select migration_id from $MIGRATION_TABLE where migration_id = '$BASE'" | $DOSQL | grep '1 row' > /dev/null
}

create () {
	echo "running migration $1"
	BASE=$(basename "$1")
	echo "creating migration $BASE"
	(
	  echo 'BEGIN;';
	  cat "$1";
	  echo "insert into $MIGRATION_TABLE(migration_id) values('$BASE');"
	  echo 'COMMIT;'
	) | $DOSQL
}

ensure () {
	echo "ensuring migration $1 has been run"
	test_ran "$1" || create "$1"
}

test_setup || setup

for migration in "$MIGRATION_DIR"/*.sql
do
	ensure "$migration"
done
