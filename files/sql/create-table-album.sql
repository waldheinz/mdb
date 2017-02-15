
CREATE TABLE album
    ( album_id      INTEGER PRIMARY KEY AUTOINCREMENT
    , album_name    TEXT
    , album_poster  INTEGER
    , album_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , FOREIGN KEY (album_poster) REFERENCES file(file_id)
    );

CREATE INDEX album_name ON album(album_name);
CREATE INDEX album_created ON album(album_created);
