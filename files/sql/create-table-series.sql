
CREATE TABLE series
    ( series_id             INTEGER PRIMARY KEY AUTOINCREMENT
    , series_name           TEXT NOT NULL
    , series_description    TEXT
    , series_poster         INTEGER REFERENCES file(file_id) ON DELETE SET NULL
    , series_tvdb_id        INTEGER
    , series_lang           TEXT                                -- language of information pulled from TheTVDB
    , series_root           TEXT UNIQUE                         -- base directory of this series relative to MDB root
    );
