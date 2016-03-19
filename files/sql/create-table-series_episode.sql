
CREATE TABLE series_episode
    ( series_id                    INTEGER NOT NULL
    , series_season_number         INTEGER NOT NULL
    , series_episode_number        INTEGER NOT NULL
    , series_episode_title         TEXT NOT NULL DEFAULT ''
    , series_episode_description   TEXT NOT NULL DEFAULT ''
    , FOREIGN KEY (series_id) REFERENCES series(series_id)
    , FOREIGN KEY (series_season_number) REFERENCES series_season(series_season_number)
    , PRIMARY KEY (series_id, series_season_number, series_episode_number)
    );
