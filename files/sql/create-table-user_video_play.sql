
CREATE TABLE user_video_play
    ( user_id           INTEGER NOT NULL REFERENCES user(user_id) ON DELETE CASCADE
    , file_id           INTEGER NOT NULL REFERENCES file(file_id) ON DELETE CASCADE
    , last_play_pos     REAL NOT NULL DEFAULT 0                                     -- position in seconds
    , last_play_time    TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL                -- when play_pos was updated
    , seen_complete     INTEGER NOT NULL DEFAULT 0                                  -- marked as "seen"
    , PRIMARY KEY (user_id, file_id)
    );
