DROP INDEX word_row_idx;
DROP INDEX words_prefixes_idx;
DROP INDEX words_prefixes_mixedcase_idx;
ALTER TABLE urls DROP CONSTRAINT urls_url_key;

ALTER TABLE urls RENAME COLUMN url TO input_url;
ALTER TABLE urls RENAME COLUMN shorturl TO short_url;
ALTER TABLE urls RENAME COLUMN headline TO title;
ALTER TABLE urls RENAME COLUMN whom TO from_nick;
ALTER TABLE urls RENAME COLUMN whence TO tstamp;

ALTER TABLE urls ADD COLUMN url_id SERIAL PRIMARY KEY;
ALTER TABLE urls ADD COLUMN redirected_url text;
ALTER TABLE urls ADD COLUMN context_id INTEGER DEFAULT 1;

DROP TABLE contexts;

CREATE TABLE contexts
    (context_id BIGSERIAL PRIMARY KEY,
     context_name TEXT,
     irc_server TEXT,
     irc_channel TEXT,
     web_server TEXT,
     web_port INTEGER,
     web_uri_prefix TEXT);

-- This configuration is for the instances on coruscant.

INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('semaphor', 'irc.srh.org', '#deepsky', 'coruscant.deepsky.com', 5749, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('bootsy', 'irc.srh.org', '#funkrehab', 'coruscant.deepsky.com', 5779, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('shogun', 'irc.srh.org', '#walled', 'coruscant.deepsky.com', 5783, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('thugster', 'irc.srh.org', '#wallednoc', 'coruscant.deepsky.com', 5801, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('harlie', 'irc.srh.org', '#trinity', 'coruscant.deepsky.com', 5791, '/');

-- Fade: For infiltrator, you'll want to substitute this:
-- INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('SR-4', 'irc.srh.org', '#trinity', 'localhost', 5791, '/');

ALTER TABLE words ADD FOREIGN KEY (context_id) REFERENCES contexts;

CREATE INDEX word_row_idx ON words (row_num);
CREATE INDEX words_prefixes_idx ON words (upper(word1), upper(word2));
CREATE INDEX words_prefixes_mixedcase_idx ON words (word1, word2);
CREATE INDEX urls_urlid_idx ON urls (url_id);
