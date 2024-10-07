insert into users (username, password_hash) values ('admin', 'admin');

insert into phrase_groups (name, group_owner) values ('Group 1', 1);

insert into phrases (text, errors, group_id, author_id)
values (
    'The quik brown fox jumps over the lasy dog.',
    '[{"word": "quik", "corrected": "quick"},{"word": "lasy", "corrected": "lazy"}]',
    1,
    1
);
