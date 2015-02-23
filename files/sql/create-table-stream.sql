CREATE TABLE stream
    ( stream_id         INTEGER NOT NULL
    , file_id           INTEGER NOT NULL
    , stream_media_type TEXT
    , stream_codec      TEXT
    , stream_bit_rate   INTEGER
    , PRIMARY KEY (stream_id, file_id)
    );
    