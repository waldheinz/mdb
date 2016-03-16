
CREATE TABLE tv_show
    ( tv_show_id            INTEGER PRIMARY KEY AUTOINCREMENT
    , tv_show_name          TEXT NOT NULL
    , tv_show_description   TEXT
    , tv_show_first_aired   TIMESTAMP
    , tv_show_poster        INTEGER
    , FOREIGN KEY (tv_show_poster) REFERENCES file(file_id)
    );
