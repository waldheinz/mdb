
CREATE TABLE tv_show_episode
    ( tv_show_id                    INTEGER NOT NULL
    , tv_show_season_number         INTEGER NOT NULL
    , tv_show_episode_number        INTEGER NOT NULL
    , tv_show_episode_title         TEXT NOT NULL DEFAULT ''
    , tv_show_episode_description   TEXT NOT NULL DEFAULT ''
    , FOREIGN KEY (tv_show_id) REFERENCES tv_show(tv_show_id)
    , FOREIGN KEY (tv_show_season_number) REFERENCES tv_show_season(tv_show_season_number)
    , PRIMARY KEY (tv_show_id, tv_show_season_number, tv_show_episode_number)
    );
