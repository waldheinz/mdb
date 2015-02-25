
CREATE TABLE album
    ( album_id      INTEGER PRIMARY KEY AUTOINCREMENT
    , album_name    TEXT
    , album_poster  INTEGER
    , FOREIGN KEY (album_poster) REFERENCES file(file_id)
    );
