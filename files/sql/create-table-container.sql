
CREATE TABLE container
    ( file_id               INTEGER PRIMARY KEY REFERENCES file(file_id) ON DELETE CASCADE
    , container_duration    REAL NOT NULL
    , container_format      TEXT NOT NULL
    );
