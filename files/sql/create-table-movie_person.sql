CREATE TABLE movie_person
    ( movie_id          INTEGER NOT NULL REFERENCES person(person_id) ON DELETE CASCADE
    , person_id         INTEGER NOT NULL REFERENCES movie(movie_id) ON DELETE CASCADE
    , movie_person_role TEXT
    , PRIMARY KEY (movie_id, person_id, movie_person_role)
    );
