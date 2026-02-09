CREATE TABLE affiliation_references (
  affiliation_reference_id UUID NOT NULL,
  item_person_id UUID,
  position INTEGER,
  affiliation VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (affiliation_reference_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1)
);
