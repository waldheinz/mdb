
CREATE TABLE files
       ( sha1		BINARY(20) NOT NULL
       , file_name	TEXT PRIMARY KEY ASC
       , file_size	INTEGER NOT NULL
       );
