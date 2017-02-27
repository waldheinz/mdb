
CREATE TABLE album_file
    ( album_id  INTEGER NOT NULL REFERENCES album(album_id) ON DELETE CASCADE
    , file_id   INTEGER NOT NULL REFERENCES file(file_id) ON DELETE CASCADE
    , PRIMARY KEY (album_id, file_id)
    );

CREATE INDEX file_to_album ON album_file(file_id, album_id);
