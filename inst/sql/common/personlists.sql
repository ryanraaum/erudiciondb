CREATE TABLE personlists (
  personlist_id UUID NOT NULL,
  item_id UUID,
  personlist_type VARCHAR,
  revision INTEGER NOT NULL,
  stage INTEGER NOT NULL,
  created TIMESTAMP DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (personlist_id, revision),
  CHECK (stage IN (0, -1)),
  CHECK (revision >= 1),
  CHECK (personlist_type IN (
    'author', 'editor', 'translator', 'director', 'performer', 'chair',
    'organizer', 'collection-editor', 'compiler', 'composer', 'container-author',
    'contributor', 'curator', 'editorial-director', 'executive-producer',
    'guest', 'host', 'interviewer', 'illustrator', 'narrator', 'original-author',
    'producer', 'recipient', 'reviewed-author', 'script-writer', 'series-creator'
  ))
);
