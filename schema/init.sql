create table if not exists users (
    id serial not null primary key,
    username varchar(255) not null unique,
    password_hash varchar(255) not null
);

create table if not exists phrase_groups (
    id serial not null primary key,
    name varchar(255) not null,
    group_owner integer not null,
    foreign key (group_owner) references users (id)
);

create table if not exists phrases (
    id serial not null primary key,
    text text not null,
    errors json,
    group_id integer not null,
    author_id integer not null,
    foreign key (group_id) references phrase_groups (id),
    foreign key (author_id) references users (id)
);
