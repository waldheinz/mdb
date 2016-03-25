
CREATE TABLE user_tag_whitelist
    ( user_id   INTEGER NOT NULL REFERENCES user(user_id) ON DELETE CASCADE
    , tag_id    INTEGER NOT NULL REFERENCES tag(tag_id) ON DELETE CASCADE
    , PRIMARY KEY (user_id, tag_id)
    );
