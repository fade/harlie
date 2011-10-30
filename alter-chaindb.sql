CREATE TABLE contexts
    (context_id BIGSERIAL PRIMARY KEY,
     context_name TEXT,
     irc_server TEXT,
     irc_channel TEXT,
     web_server TEXT,
     web_port INTEGER,
     web_uri_prefix TEXT);

INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('semaphor', 'irc.srh.org', '#deepsky', '127.0.0.1', 5749, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('bootsy', 'irc.srh.org', '#funkrehab', '127.0.0.1', 5779, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('shogun', 'irc.srh.org', '#walled', '127.0.0.1', 5783, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('thugster', 'irc.srh.org', '#wallednoc', '127.0.0.1', 5801, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('harlot', 'irc.srh.org', '#trinity', '127.0.0.1', 5807, '/');
INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('harlie', 'irc.srh.org', '#trinity', '127.0.0.1', 5791, '/');

ALTER TABLE words ADD COLUMN row_num BIGSERIAL;

ALTER TABLE words ADD COLUMN context_id INTEGER DEFAULT [n];

ALTER TABLE words ADD FOREIGN KEY (context_id) REFERENCES contexts;

CREATE INDEX word_row_idx ON words (row_num);

CREATE INDEX words_prefixes_idx ON words (upper(word1), upper(word2));

CREATE INDEX words_prefixes_mixedcase_idx ON words (word1, word2);
