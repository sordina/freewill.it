
create table users(
	userid    integer PRIMARY KEY,
	firstname text,
	lastname  text
);

create table choices(
	choiceid   integer primary key,
	userid     integer references users (userid),
	choicename text
);

create table options(
	optionid       integer primary key,
	userid         integer references users (userid),
	optionchoiceid integer references choices (choiceid),
	optionname     text
);

create table decisions(
	decisionid       integer primary key,
	userid           integer references users (userid),
	decisionchoiceid integer references choices (choiceid)
);
