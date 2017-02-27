
CREATE TABLE stream_metadata
    ( stream_metadata_id    INTEGER PRIMARY KEY AUTOINCREMENT
    , stream_id             INTEGER NOT NULL
    , file_id               INTEGER NOT NULL
    , stream_metadata_key   TEXT NOT NULL
    , stream_metadata_value TEXT NOT NULL
    , FOREIGN KEY (stream_id, file_id) REFERENCES stream(stream_id, file_id) ON DELETE CASCADE
    );
