CREATE TABLE issues (
  issue_id UUID NOT NULL,
  status VARCHAR,
  object_type VARCHAR,
  object_id UUID,
  description VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (issue_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1)
);
