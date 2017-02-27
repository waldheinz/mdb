
CREATE TABLE album
    ( album_id      INTEGER PRIMARY KEY AUTOINCREMENT
    , album_name    TEXT
    , album_poster  INTEGER REFERENCES file(file_id) ON DELETE SET NULL
    , album_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    );

CREATE INDEX album_name ON album(album_name);
CREATE INDEX album_created ON album(album_created);
