CREATE TABLE contexts (
    context_id bigint NOT NULL,
    context_name text,
    irc_server text,
    irc_channel text,
    web_server text,
    web_port integer,
    web_uri_prefix text
);

ALTER TABLE public.contexts OWNER TO #laughingboy#;

CREATE SEQUENCE contexts_context_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contexts_context_id_seq OWNER TO #laughingboy#;

ALTER SEQUENCE contexts_context_id_seq OWNED BY contexts.context_id;

CREATE TABLE urls (
    title text,
    input_url text,
    short_url text,
    from_nick text,
    tstamp integer,
    url_id integer NOT NULL,
    redirected_url text,
    context_id integer DEFAULT 1,
    dead boolean DEFAULT false
);


ALTER TABLE public.urls OWNER TO #laughingboy#;

CREATE SEQUENCE urls_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.urls_url_id_seq OWNER TO #laughingboy#;

ALTER SEQUENCE urls_url_id_seq OWNED BY urls.url_id;

CREATE TABLE words (
    word1 text,
    word2 text,
    word3 text,
    incidence integer,
    row_num integer NOT NULL,
    context_id integer DEFAULT 1
);


ALTER TABLE public.words OWNER TO #laughingboy#;

CREATE SEQUENCE words_row_num_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.words_row_num_seq OWNER TO #laughingboy#;

ALTER SEQUENCE words_row_num_seq OWNED BY words.row_num;

ALTER TABLE ONLY contexts ALTER COLUMN context_id SET DEFAULT nextval('contexts_context_id_seq'::regclass);

ALTER TABLE ONLY urls ALTER COLUMN url_id SET DEFAULT nextval('urls_url_id_seq'::regclass);

ALTER TABLE ONLY words ALTER COLUMN row_num SET DEFAULT nextval('words_row_num_seq'::regclass);

ALTER TABLE ONLY contexts
    ADD CONSTRAINT contexts_pkey PRIMARY KEY (context_id);

ALTER TABLE ONLY urls
    ADD CONSTRAINT urls_pkey PRIMARY KEY (url_id);

ALTER TABLE ONLY urls
    ADD CONSTRAINT urls_unique_shorturl UNIQUE (short_url);

CREATE INDEX urls_urlid_idx ON urls USING btree (url_id);

CREATE INDEX word_row_idx ON words USING btree (row_num);

CREATE INDEX words_contexts_idx ON words USING btree (context_id);

CREATE INDEX words_prefixes_idx ON words USING btree (upper(word1), upper(word2));

ALTER TABLE ONLY urls
    ADD CONSTRAINT urls_context_id_fkey FOREIGN KEY (context_id) REFERENCES contexts(context_id);

ALTER TABLE ONLY words
    ADD CONSTRAINT words_context_id_fkey FOREIGN KEY (context_id) REFERENCES contexts(context_id);
