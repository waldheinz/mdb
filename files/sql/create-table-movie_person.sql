CREATE TABLE movie_person
    ( movie_id          INTEGER NOT NULL
    , person_id         INTEGER NOT NULL
    , movie_person_role TEXT
    , PRIMARY KEY (movie_id, person_id, movie_person_role)
    , FOREIGN KEY (person_id) REFERENCES person(person_id)
    , FOREIGN KEY (movie_id) REFERENCES movie(movie_id)
    );
