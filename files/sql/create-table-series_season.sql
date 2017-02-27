
CREATE TABLE series_season
    ( series_id            INTEGER NOT NULL REFERENCES series(series_id) ON DELETE CASCADE
    , series_season_number INTEGER NOT NULL
    , series_season_poster INTEGER REFERENCES file(file_id) ON DELETE SET NULL
    , PRIMARY KEY (series_id, series_season_number)
    );
