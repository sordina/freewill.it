
create table users(
	userid    serial PRIMARY KEY,
	firstname text,
	lastname  text
);

-- choiceid   uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
create table choices(
	choiceid   serial primary key,
	userid     integer references users (userid),
	choicename text
);

create table options(
	optionid       serial primary key,
	userid         integer references users (userid),
	optionchoiceid integer references choices (choiceid),
	optionname     text
);

create table decisions(
	decisionid       serial primary key,
	userid           integer references users   (userid),
	decisionchoiceid integer references choices (choiceid),
	decision         integer references options (optionid)
);
