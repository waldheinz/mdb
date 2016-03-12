
CREATE TABLE user_session
    ( session_id        BINARY(16) PRIMARY KEY
    , user_id           INTEGER NOT NULL
    , session_created   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , session_expires   TIMESTAMP
    , FOREIGN KEY (user_id) REFERENCES user(user_id)
    );
