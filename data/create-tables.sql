
CREATE TABLE files
    ( file_id   INTEGER PRIMARY KEY AUTOINCREMENT
    , file_sha1 BINARY(20)
    , file_name TEXT UNIQUE
    , file_size INTEGER NOT NULL
    );
