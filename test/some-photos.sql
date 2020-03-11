create table if not exists photos (
        id integer primary key autoincrement,
        file_path text not null unique
);

