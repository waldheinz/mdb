
CREATE TABLE user_session
    ( session_id        TEXT PRIMARY KEY
    , user_id           INTEGER NOT NULL REFERENCES user(user_id) ON DELETE CASCADE
    , session_created   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    );
