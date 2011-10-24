ALTER TABLE words ADD COLUMN row_num BIGSERIAL;

CREATE INDEX word_row_idx ON words (row_num);

CREATE INDEX words_prefixes_idx ON words (upper(word1), upper(word2));
