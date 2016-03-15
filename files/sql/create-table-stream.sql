CREATE TABLE stream
    ( stream_id         INTEGER NOT NULL
    , file_id           INTEGER NOT NULL
    , stream_media_type TEXT
    , stream_codec      TEXT
    , stream_bit_rate   INTEGER
    , stream_width      INTEGER
    , stream_height     INTEGER
    , PRIMARY KEY (stream_id, file_id)
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
