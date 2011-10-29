CREATE TABLE contexts (context_id BIGSERIAL PRIMARY KEY, context_name TEXT);

INSERT INTO contexts (context_name) VALUES ('semaphor');
INSERT INTO contexts (context_name) VALUES ('bootsy');
INSERT INTO contexts (context_name) VALUES ('shogun');
INSERT INTO contexts (context_name) VALUES ('thugster');
INSERT INTO contexts (context_name) VALUES ('harlie');

ALTER TABLE words ADD COLUMN row_num BIGSERIAL;

ALTER TABLE words ADD COLUMN context_id INTEGER DEFAULT [n];

ALTER TABLE words ADD FOREIGN KEY (context_id) REFERENCES contexts;

CREATE INDEX word_row_idx ON words (row_num);

CREATE INDEX words_prefixes_idx ON words (upper(word1), upper(word2));

CREATE INDEX words_prefixes_mixedcase_idx ON words (word1, word2);
