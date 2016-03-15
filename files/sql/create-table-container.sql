
CREATE TABLE container
    ( file_id               INTEGER PRIMARY KEY
    , container_duration    REAL NOT NULL
    , container_format      TEXT NOT NULL
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
