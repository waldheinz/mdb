
CREATE TABLE tag_file
    ( tag_id    INTEGER NOT NULL REFERENCES tag(tag_id) ON DELETE CASCADE
    , file_id   INTEGER NOT NULL REFERENCES file(file_id) ON DELETE CASCADE
    , PRIMARY KEY (tag_id, file_id)
    );

CREATE INDEX file_to_tag ON tag_file(file_id, tag_id);
