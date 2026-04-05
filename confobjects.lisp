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

;;; Each IRC connection is a self-contained unit.  connection-spec carries
;;; everything one bot personality needs: server, nick, channel, and the
;;; web port that serves its URL-shortener context.

(defclass connection-spec ()
  ((server            :initarg :server            :accessor cs-server)
   (ssl               :initarg :ssl               :accessor cs-ssl
                      :initform nil)
   (nick              :initarg :nick              :accessor cs-nick)
   (channel           :initarg :channel           :accessor cs-channel)
   (channel-key       :initarg :channel-key       :accessor cs-channel-key
                      :initform nil)
   (nickserv-password :initarg :nickserv-password :accessor cs-nickserv-password
                      :initform nil)
   (nickserv-email    :initarg :nickserv-email    :accessor cs-nickserv-email
                      :initform nil)
   (extra-channels    :initarg :extra-channels    :accessor cs-extra-channels
                      :initform nil
                      :documentation "List of additional channels to join on this connection.")
   (web-port          :initarg :web-port          :accessor cs-web-port)))

(defun make-connection-spec (&rest args)
  (apply #'make-instance 'connection-spec args))

;;; Top-level bot configuration: one database credential set, the web
;;; hostname, and a flat list of connection-spec objects.

(defclass config ()
  ((db-credentials  :initarg :db-credentials  :accessor db-credentials)
   (web-server-name :initarg :web-server-name :accessor web-server-name)
   (connections     :initarg :connections     :accessor connections)))

(defun make-config (&rest args)
  (apply #'make-instance 'config args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; confobjects.lisp ends here
