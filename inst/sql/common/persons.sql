CREATE TABLE persons (
  person_id UUID NOT NULL,
  primary_person_id UUID,
  also_known_as BOOLEAN,
  primary_given_names VARCHAR,
  other_given_names VARCHAR,
  ascii_given_names VARCHAR,
  surnames VARCHAR,
  ascii_surnames VARCHAR,
  dropping_particle VARCHAR,
  non_dropping_particle VARCHAR,
  prefix VARCHAR,
  suffix VARCHAR,
  comma_suffix BOOLEAN,
  short_referent VARCHAR,
  long_referent VARCHAR,
  sorting_referent VARCHAR,
  name_in_origin_script VARCHAR,
  subjective_pronoun VARCHAR,
  objective_pronoun VARCHAR,
  possessive_pronoun VARCHAR,
  status VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (person_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1),
  CHECK (
    (primary_given_names IS NOT NULL AND primary_given_names != '') OR
    (other_given_names IS NOT NULL AND other_given_names != '') OR
    (surnames IS NOT NULL AND surnames != '')
  )
);
