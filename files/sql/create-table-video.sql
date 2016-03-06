
CREATE TABLE video
    ( video_id          INTEGER PRIMARY KEY AUTOINCREMENT
    , file_id           INTEGER NOT NULL
    , video_bit_rate    INTEGER NOT NULL
    , video_duration    REAL NOT NULL
    , video_format      TEXT NOT NULL
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
