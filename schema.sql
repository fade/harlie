--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: contexts; Type: TABLE; Schema: public; Owner: fade; Tablespace: 
--

CREATE TABLE contexts (
    context_id bigint NOT NULL,
    context_name text,
    irc_server text,
    irc_channel text,
    web_server text,
    web_port integer,
    web_uri_prefix text
);


ALTER TABLE public.contexts OWNER TO fade;

--
-- Name: contexts_context_id_seq; Type: SEQUENCE; Schema: public; Owner: fade
--

CREATE SEQUENCE contexts_context_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.contexts_context_id_seq OWNER TO fade;

--
-- Name: contexts_context_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: fade
--

ALTER SEQUENCE contexts_context_id_seq OWNED BY contexts.context_id;


--
-- Name: urls; Type: TABLE; Schema: public; Owner: fade; Tablespace: 
--

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


ALTER TABLE public.urls OWNER TO fade;

--
-- Name: urls_url_id_seq; Type: SEQUENCE; Schema: public; Owner: fade
--

CREATE SEQUENCE urls_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.urls_url_id_seq OWNER TO fade;

--
-- Name: urls_url_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: fade
--

ALTER SEQUENCE urls_url_id_seq OWNED BY urls.url_id;


--
-- Name: words; Type: TABLE; Schema: public; Owner: fade; Tablespace: 
--

CREATE TABLE words (
    word1 text,
    word2 text,
    word3 text,
    incidence integer,
    row_num integer NOT NULL,
    context_id integer DEFAULT 1
);


ALTER TABLE public.words OWNER TO fade;

--
-- Name: words_row_num_seq; Type: SEQUENCE; Schema: public; Owner: fade
--

CREATE SEQUENCE words_row_num_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.words_row_num_seq OWNER TO fade;

--
-- Name: words_row_num_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: fade
--

ALTER SEQUENCE words_row_num_seq OWNED BY words.row_num;


--
-- Name: context_id; Type: DEFAULT; Schema: public; Owner: fade
--

ALTER TABLE ONLY contexts ALTER COLUMN context_id SET DEFAULT nextval('contexts_context_id_seq'::regclass);


--
-- Name: url_id; Type: DEFAULT; Schema: public; Owner: fade
--

ALTER TABLE ONLY urls ALTER COLUMN url_id SET DEFAULT nextval('urls_url_id_seq'::regclass);


--
-- Name: row_num; Type: DEFAULT; Schema: public; Owner: fade
--

ALTER TABLE ONLY words ALTER COLUMN row_num SET DEFAULT nextval('words_row_num_seq'::regclass);


--
-- Name: contexts_pkey; Type: CONSTRAINT; Schema: public; Owner: fade; Tablespace: 
--

ALTER TABLE ONLY contexts
    ADD CONSTRAINT contexts_pkey PRIMARY KEY (context_id);


--
-- Name: urls_pkey; Type: CONSTRAINT; Schema: public; Owner: fade; Tablespace: 
--

ALTER TABLE ONLY urls
    ADD CONSTRAINT urls_pkey PRIMARY KEY (url_id);


--
-- Name: urls_urlid_idx; Type: INDEX; Schema: public; Owner: fade; Tablespace: 
--

CREATE INDEX urls_urlid_idx ON urls USING btree (url_id);


--
-- Name: word_row_idx; Type: INDEX; Schema: public; Owner: fade; Tablespace: 
--

CREATE INDEX word_row_idx ON words USING btree (row_num);


--
-- Name: words_prefixes_idx; Type: INDEX; Schema: public; Owner: fade; Tablespace: 
--

CREATE INDEX words_prefixes_idx ON words USING btree (upper(word1), upper(word2));


--
-- Name: words_prefixes_mixedcase_idx; Type: INDEX; Schema: public; Owner: fade; Tablespace: 
--

CREATE INDEX words_prefixes_mixedcase_idx ON words USING btree (word1, word2);


--
-- Name: urls_context_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: fade
--

ALTER TABLE ONLY urls
    ADD CONSTRAINT urls_context_id_fkey FOREIGN KEY (context_id) REFERENCES contexts(context_id);


--
-- Name: words_context_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: fade
--

ALTER TABLE ONLY words
    ADD CONSTRAINT words_context_id_fkey FOREIGN KEY (context_id) REFERENCES contexts(context_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;

INSERT INTO contexts (context_name, irc_server, irc_channel, web_server, web_port, web_uri_prefix) VALUES ('SR-4', 'irc.srh.org', '#trinity', 'localhost', 5791, '/');

--
-- PostgreSQL database dump complete
--

