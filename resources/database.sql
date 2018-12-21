
begin;

create type sex as enum ('f', 'm');

create type status as enum ('свободны', 'заняты', 'всё сложно');

create table accounts (
    id        serial   primary key,
    email     text     not null unique,
    fname     text     null,
    sname     text     null,
    phone     text     null,
    sex       sex      not null,
    birth     integer  not null,
    country   text     null,
    city      text     null,
    joined    integer  not null,
    status    status   not null,
    interests text[]   null,
    premium   jsonb    not null,
    likes     jsonb    not null
);


commit;
