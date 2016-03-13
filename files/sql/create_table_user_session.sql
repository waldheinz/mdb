
CREATE TABLE user_session
    ( session_id        TEXT PRIMARY KEY
    , user_id           INTEGER NOT NULL
    , session_created   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , FOREIGN KEY (user_id) REFERENCES user(user_id)
    );
