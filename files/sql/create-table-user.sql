
CREATE TABLE user
    ( user_id           INTEGER PRIMARY KEY AUTOINCREMENT
    , user_name         TEXT NOT NULL UNIQUE
    , user_pass_scrypt  TEXT
    , user_add_time     TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
    , user_last_login   TIMESTAMP
    );
