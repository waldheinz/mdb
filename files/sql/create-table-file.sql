
CREATE TABLE file
    ( file_id   INTEGER PRIMARY KEY AUTOINCREMENT
    , file_sha1 BINARY(20)
    , file_name TEXT UNIQUE
    , file_size INTEGER NOT NULL
    , file_mime TEXT NOT NULL
    );
