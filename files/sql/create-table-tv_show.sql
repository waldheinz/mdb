
CREATE TABLE tv_show
    ( tv_show_id            INTEGER PRIMARY KEY AUTOINCREMENT
    , tv_show_name          TEXT NOT NULL
    , tv_show_description   TEXT
    , tv_show_poster        INTEGER
    , tv_show_tvdb_id       INTEGER
    , tv_show_root          TEXT UNIQUE
    , FOREIGN KEY (tv_show_poster) REFERENCES file(file_id)
    );
