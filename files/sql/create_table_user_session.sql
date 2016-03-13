
CREATE TABLE user_session
    ( session_id        BINARY PRIMARY KEY
    , user_id           INTEGER
    , session_created   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , FOREIGN KEY (user_id) REFERENCES user(user_id)
    );
