
CREATE TABLE video
    ( video_id          INTEGER PRIMARY KEY AUTOINCREMENT
    , file_id           INTEGER NOT NULL
    , video_bitrate     INTEGER NOT NULL
    , video_duration    REAL NOT NULL
    , video_format      TEXT NOT NULL
    , video_scan_time   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
