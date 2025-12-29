;;; confobjects.lisp --- 
;; 
;; Filename: confobjects.lisp
;; Description: 
;; Author: Brian O'Reilly
;; Maintainer: 
;; Created: Mon Dec 12 14:16:28 2011 (-0500)
;; Version: 
;; Last-Updated: Mon Dec 12 14:50:35 2011 (-0500)
;;           By: Brian O'Reilly
;;     Update #: 6
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(in-package :harlie)

;;; a transliteration to a CLOS class of the struct that formerly
;;; contained the configuration information for the various channel
;;; personalities of a given image containing a harlie instance.

(defclass config ()
  ((irc-server-name :initarg :irc-server-name :accessor irc-server-name)
   (irc-channel-names :initarg :irc-channel-names :accessor irc-channel-names)
   (irc-joins :initarg :irc-joins :accessor irc-joins)
   (ssl :initarg :ssl :initform :none :accessor ssl)
   (irc-nickchannels :initarg :irc-nickchannels :accessor irc-nickchannels)
   (web-server-name :initarg :web-server-name :accessor web-server-name)
   (web-server-ports :initarg :web-server-ports :accessor web-server-ports)
   (url-store-type :initarg :url-store-type :accessor url-store-type)
   (psql-old-credentials :initarg :psql-old-credentials :initform nil :accessor psql-old-credentials)
   (psql-url-new-credentials :initarg :psql-url-new-credentials :accessor psql-url-new-credentials)
   (psql-chain-credentials :initarg :psql-chain-credentials :accessor psql-chain-credentials)
   (psql-context-credentials :initarg :psql-context-credentials :accessor psql-context-credentials)
   (psql-botdb-credentials :initarg :psql-botdb-credentials :initform nil :accessor psql-botdb-credentials)))

(defun make-config (&rest args)
  (apply #'make-instance 'config args))

(defmethod initialize-instance :after ((conf config) &key)
  (with-accessors ((botdb psql-botdb-credentials) (ctxt psql-context-credentials))
      conf
    (when (not botdb)
      (setf botdb ctxt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; confobjects.lisp ends here
