
CREATE TABLE person_file
    ( person_id INTEGER NOT NULL REFERENCES person(person_id) ON DELETE CASCADE
    , file_id INTEGER NOT NULL REFERENCES file(file_id) ON DELETE CASCADE
    , PRIMARY KEY (person_id, file_id)
    );
