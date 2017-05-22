
create table users (
	userid    uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
	created   timestamp default now(),
	email     text,
	password  text
);

create table choices (
	choiceid   uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
	created    timestamp default now(),
	userid     uuid references users (userid),
	choicename text
);

create table options (
	optionid       uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
	created        timestamp default now(),
	userid         uuid references users (userid),
	optionchoiceid uuid references choices (choiceid),
	optionname     text
);

create table decisions (
	decisionid       uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
	created          timestamp default now(),
	userid           uuid references users   (userid),
	decisionchoiceid uuid references choices (choiceid),
	decision         uuid references options (optionid)
);
