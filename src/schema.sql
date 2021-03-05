create table if not exists Problems 
( id serial primary key
, name varchar not null
, n_inputs int not null default 0
, description text not null
, submitted_at timestamptz default current_timestamp
, solution_type varchar not null 
);

create table if not exists Users
( id serial primary key
, group_id int not null
, snowflake varchar(64) not null
, score int not null
, solved int not null
, unique(snowflake)
);

create table if not exists Inputs
( problem_id integer references Problems(id)
, group_id int not null
, input json not null
, answer json not null
);

create or replace function inc_ref_count() returns trigger as $$
    begin
        update Problems set n_inputs = n_inputs + 1
        where id = NEW.problem_id;
        return NEW;
    end;
$$
language plpgsql volatile;

create trigger inc_ref_count
    after insert on Inputs for each row execute procedure inc_ref_count();