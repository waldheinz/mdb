
CREATE TABLE album_file
    ( album_id  INTEGER NOT NULL
    , file_id   INTEGER NOT NULL
    , PRIMARY KEY (album_id, file_id)
    , FOREIGN KEY (album_id) REFERENCES album(album_id)
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
