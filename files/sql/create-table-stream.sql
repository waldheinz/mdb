CREATE TABLE stream
    ( stream_id         INTEGER NOT NULL
    , video_id          INTEGER NOT NULL
    , stream_media_type TEXT
    , stream_codec      TEXT
    , stream_bit_rate   INTEGER
    , PRIMARY KEY (stream_id, video_id)
    , FOREIGN KEY (video_id) REFERENCES video(video_id)
    );
