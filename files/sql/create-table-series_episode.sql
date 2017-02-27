
CREATE TABLE series_episode
    ( series_id                     INTEGER NOT NULL REFERENCES series(series_id) ON DELETE CASCADE
    , series_season_number          INTEGER NOT NULL
    , series_episode_number         INTEGER NOT NULL
    , series_episode_title          TEXT NOT NULL DEFAULT ''
    , series_episode_description    TEXT NOT NULL DEFAULT ''
    , file_id                       INTEGER REFERENCES file(file_id) ON DELETE SET NULL
    , FOREIGN KEY (series_season_number, series_id)
        REFERENCES series_season(series_season_number, series_id)
        ON DELETE CASCADE
    , PRIMARY KEY (series_id, series_season_number, series_episode_number)
    );
