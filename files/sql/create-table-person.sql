
CREATE TABLE person
    ( person_id         INTEGER PRIMARY KEY AUTOINCREMENT
    , person_name       TEXT UNIQUE
    , person_portrait   INTEGER
    , FOREIGN KEY (person_portrait) REFERENCES file(file_id)
    );
