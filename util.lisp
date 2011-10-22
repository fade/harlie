;;;; util.lisp

(in-package #:harlie)

(defun string-strip-surrounding-whitespace (str)
  "Strip whitespace characters from beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Return #\Tab) str))

(defun strip-spaces (strings)
  "remove book-end whitespace from a string or list of strings."
  (cond
    ((listp strings) (mapcar #'string-strip-surrounding-whitespace strings))
    (t (string-strip-surrounding-whitespace strings))))

(defun string-remove-embedded-newlines (str)
  "Remove newline or carriage return characters from a string."
  (concatenate 'string (loop for c across str when (not (or (eq c #\Newline) (eq c #\Return))) collecting c)))

(defun cleanup-title (title)
  "Remove extraneous whitespace characters from within and around a string."
  (string-strip-surrounding-whitespace (string-remove-embedded-newlines title)))

(defun float-as-string-p (fstring)
  "Is fstring a floating point number encoded as a string?"
  (let ((scanner (create-scanner "^[0-9]*([.][0-9]*)?$")))
    (cl-ppcre:scan scanner fstring)))