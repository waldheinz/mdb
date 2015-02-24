
CREATE TABLE person_file
    ( person_id INTEGER NOT NULL
    , file_id INTEGER NOT NULL
    , PRIMARY KEY (person_id, file_id)
    , FOREIGN KEY (person_id) REFERENCES person(person_id)
    , FOREIGN KEY (file_id) REFERENCES file(file_id)
    );
