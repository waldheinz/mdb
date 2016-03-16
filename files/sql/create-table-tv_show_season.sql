
CREATE TABLE tv_show_season
    ( tv_show_id            INTEGER NOT NULL
    , tv_show_season_number INTEGER NOT NULL
    , tv_show_season_poster INTEGER
    , FOREIGN KEY (tv_show_id) REFERENCES tv_show(tv_show_id)
    , FOREIGN KEY (tv_show_season_poster) REFERENCES file(file_id)
    , PRIMARY KEY (tv_show_id, tv_show_season_number)
    );
