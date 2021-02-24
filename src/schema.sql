create schema if not exists CCData;

create table if not exists Problems 
( id serial primary key
, name varchar not null
, description text not null
, submitted_at timestamptz not null
);

create table if not exists Users
( id serial primary key
, group_id int not null
, discord_name varchar(64) not null
, score int not null
, solved int not null
);

create table if not exists Inputs
( problem_id integer references Problems(id)
, group_id int not null
, input json not null
, answer int not null
)