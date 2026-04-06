;;;; test/unit/urls.lisp
;;;;
;;;; Unit tests for URL extraction and UTM-parameter stripping.
;;;; No network, no database, no side effects.

(uiop:define-package #:harlie/test/unit/urls
  (:use #:cl)
  (:import-from #:harlie
                #:extract-urls
                #:de-utm-url
                #:get-filename-from-url)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:harlie/test/unit/urls)

(tt:define-test "harlie.unit.urls")

;;;; ---- extract-urls -----------------------------------------------------

(tt:define-test "extract-urls"
  :parent "harlie.unit.urls"

  ;; basic http
  (tt:is equal '("http://example.com")
         (extract-urls "see http://example.com for details"))

  ;; https
  (tt:is equal '("https://example.com/path?q=1")
         (extract-urls "check https://example.com/path?q=1 now"))

  ;; www-style (no scheme)
  (tt:is equal '("www.example.com")
         (extract-urls "visit www.example.com please"))

  ;; multiple URLs in one string
  (tt:is = 2
         (length (extract-urls "http://a.com and https://b.org/page")))

  ;; URL at end of sentence (trailing punctuation not part of the URL)
  (tt:true (extract-urls "go to http://example.com."))

  ;; no URLs
  (tt:is equal '()
         (extract-urls "nothing to see here"))

  ;; empty string
  (tt:is equal '()
         (extract-urls "")))

;;;; ---- de-utm-url --------------------------------------------------------

(tt:define-test "de-utm-url"
  :parent "harlie.unit.urls"

  ;; strips utm_source as query-string start
  (tt:is string= "https://example.com/page"
         (de-utm-url "https://example.com/page?utm_source=twitter"))

  ;; strips utm_medium after another parameter
  (tt:is string= "https://example.com/page?foo=bar"
         (de-utm-url "https://example.com/page?foo=bar&utm_medium=social"))

  ;; strips mbid tracking parameter
  (tt:is string= "https://example.com/page"
         (de-utm-url "https://example.com/page?mbid=social_twitter"))

  ;; case-insensitive: UTM_ (uppercase)
  (tt:is string= "https://example.com/page"
         (de-utm-url "https://example.com/page?UTM_source=google"))

  ;; case-insensitive: MbId (mixed)
  (tt:is string= "https://example.com/"
         (de-utm-url "https://example.com/?MbId=123"))

  ;; no tracking params — returned unchanged
  (tt:is string= "https://example.com/page?foo=bar"
         (de-utm-url "https://example.com/page?foo=bar"))

  ;; plain URL with no query string — unchanged
  (tt:is string= "https://example.com/page"
         (de-utm-url "https://example.com/page"))

  ;; empty string — unchanged
  (tt:is string= ""
         (de-utm-url "")))

;;;; ---- get-filename-from-url -----------------------------------------------

(tt:define-test "get-filename-from-url"
  :parent "harlie.unit.urls"

  ;; simple binary filename
  (tt:is string= "archive.tar.gz"
         (get-filename-from-url "https://example.com/files/archive.tar.gz"))

  ;; filename with no directory
  (tt:is string= "image.iso"
         (get-filename-from-url "https://example.com/image.iso"))

  ;; deep path
  (tt:is string= "package.deb"
         (get-filename-from-url "https://mirror.example.com/debian/pool/main/p/pkg/package.deb"))

  ;; URL with query string — filename still extracted from path
  (tt:is string= "release.zip"
         (get-filename-from-url "https://example.com/downloads/release.zip?token=abc123"))

  ;; URL ending in slash — no filename
  (tt:is string= ""
         (get-filename-from-url "https://example.com/path/")))
