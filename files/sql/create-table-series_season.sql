
CREATE TABLE series_season
    ( series_id            INTEGER NOT NULL
    , series_season_number INTEGER NOT NULL
    , series_season_poster INTEGER
    , FOREIGN KEY (series_id) REFERENCES series(series_id)
    , FOREIGN KEY (series_season_poster) REFERENCES file(file_id)
    , PRIMARY KEY (series_id, series_season_number)
    );
