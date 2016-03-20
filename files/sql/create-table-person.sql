
CREATE TABLE person
    ( person_id         INTEGER PRIMARY KEY AUTOINCREMENT
    , person_name       TEXT UNIQUE
    , person_portrait   INTEGER REFERENCES file(file_id) ON DELETE SET NULL
    );
