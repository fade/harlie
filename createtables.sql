create table instances (
    instance_id bigserial primary key,
    instance_name text);

create table urls (
    url_id bigserial primary key,
    input_url text,
    redirected_url text,
    short_url text,
    title text,
    from_nick text,
    tstamp integer,
    instance_id integer not null references instances(instance_id));

insert into instances (instance_name) values ('semaphor');
insert into instances (instance_name) values ('bootsy');
insert into instances (instance_name) values ('shogun');
insert into instances (instance_name) values ('thugster');
insert into instances (instance_name) values ('harlie');
