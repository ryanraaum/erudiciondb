CREATE TABLE item_person_identifiers (
  item_person_identifier_id UUID NOT NULL,
  item_person_id UUID,
  id_type VARCHAR,
  id_value VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (item_person_identifier_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1)
);
