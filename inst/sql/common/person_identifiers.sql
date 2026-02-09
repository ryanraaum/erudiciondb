CREATE TABLE person_identifiers (
  person_identifier_id UUID NOT NULL,
  person_id UUID,
  id_type VARCHAR,
  id_value VARCHAR,
  id_value_uppercase VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (person_identifier_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1)
);
